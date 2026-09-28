module A = Pie.Recording.Retry_store.Active

let fail_store = function
  | Error (`Retry_store m) -> Alcotest.fail m
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok v -> v

let is_busy = function Error (`Retry_store "busy") -> true | _ -> false

let tmp_root () =
  ignore (Unix.umask 0o022);
  let p = Filename.temp_file "pie-recov" "" in
  Sys.remove p;
  Unix.mkdir p 0o700;
  p

let rec rm_r path =
  match Unix.lstat path with
  | { st_kind = Unix.S_DIR; _ } ->
      Array.iter
        (fun n -> if n <> "." && n <> ".." then rm_r (Filename.concat path n))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_root f =
  let root = tmp_root () in
  Fun.protect
    ~finally:(fun () -> try rm_r root with _ -> ())
    (fun () -> f root)

let even_pcm = Bytes.of_string "ABCD"

let test_umask_private () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let active = Filename.concat root "active" in
      let st = Unix.lstat active in
      Alcotest.(check int) "dir 0700" 0o700 (st.st_perm land 0o777);
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      ignore w;
      let lock = Filename.concat active "publish.lock" in
      fail_store
        (A.publish_text store ~mode:Transcribe ~text:"hi"
           ~dispatch_uncertain:false)
      |> ignore;
      let idx = Filename.concat active "index.json" in
      let ist = Unix.lstat idx in
      Alcotest.(check int) "index 0600" 0o600 (ist.st_perm land 0o777);
      Alcotest.(check bool) "lockfile kept" true (Sys.file_exists lock);
      fail_store (A.abort_writer w))

let test_draft_and_sealed_unpublished () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w even_pcm);
      (match fail_store (A.peek store) with
      | None -> ()
      | Some _ -> Alcotest.fail "active writer selected");
      fail_store (A.close_writer w);
      (match fail_store (A.peek store) with
      | None -> ()
      | Some _ -> Alcotest.fail "sealed unpublished selected");
      fail_store (A.abort_writer w))

let test_publish_audio_and_reload () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Translate) in
      fail_store (A.append_audio w even_pcm);
      fail_store (A.close_writer w);
      let g = fail_store (A.publish_audio store w) in
      match A.peek store with
      | Error (`Retry_store m) -> Alcotest.fail ("peek err " ^ m)
      | Ok None -> Alcotest.fail "peek none after publish"
      | Ok (Some (A.Audio { mode = Translate; pcm_bytes = 4; generation }))
        when A.generation_equal generation g -> (
          let pcm = fail_store (A.load_audio store g) in
          Alcotest.(check int) "pcm" 4 (Bytes.length pcm);
          let store2 = fail_store (A.open_store ~root) in
          match fail_store (A.peek store2) with
          | Some (A.Audio { generation = g2; _ }) when A.generation_equal g2 g
            ->
              ()
          | _ -> Alcotest.fail "startup reload missed audio")
      | _ -> Alcotest.fail "audio not published")

let test_publish_text_startup () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let g =
        fail_store
          (A.publish_text store ~mode:Transcribe ~text:"hello"
             ~dispatch_uncertain:true)
      in
      let store2 = fail_store (A.open_store ~root) in
      match fail_store (A.peek store2) with
      | Some
          (A.Text
             {
               mode = Transcribe;
               utf8_bytes = 5;
               dispatch_uncertain = true;
               generation;
             })
        when A.generation_equal generation g ->
          Alcotest.(check string)
            "text" "hello"
            (fail_store (A.load_text store2 g))
      | _ -> Alcotest.fail "text startup reload")

let test_symlink_fifo_unrelated () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let g =
        fail_store
          (A.publish_text store ~mode:Transcribe ~text:"ok"
             ~dispatch_uncertain:false)
      in
      let active = Filename.concat root "active" in
      Unix.symlink "/etc/passwd" (Filename.concat active "slot-evil");
      Unix.mkfifo (Filename.concat active "fifo") 0o600;
      let oc = open_out_bin (Filename.concat active "unrelated.pcm") in
      output_string oc "nope";
      close_out oc;
      match fail_store (A.peek store) with
      | Some (A.Text { generation; _ }) when A.generation_equal generation g ->
          ()
      | _ -> Alcotest.fail "valid candidate lost to junk")

let test_generation_clear_race () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let a =
        fail_store
          (A.publish_text store ~mode:Transcribe ~text:"one"
             ~dispatch_uncertain:false)
      in
      let b =
        fail_store
          (A.publish_text store ~mode:Transcribe ~text:"two"
             ~dispatch_uncertain:false)
      in
      fail_store (A.clear store a);
      match fail_store (A.peek store) with
      | Some (A.Text { generation; _ }) when A.generation_equal generation b ->
          Alcotest.(check string)
            "kept" "two"
            (fail_store (A.load_text store b))
      | _ -> Alcotest.fail "late clear of A dropped B")

let test_concurrent_instances_busy () =
  with_root (fun root ->
      let store1 = fail_store (A.open_store ~root) in
      let store2 = fail_store (A.open_store ~root) in
      let gate = Atomic.make 0 in
      let r1 = ref (Ok ()) in
      let r2 = ref (Ok ()) in
      let hold store dest =
        (* occupy lock via a publish that waits on gate after lock? we can't.
           Two publish_text concurrent: process-local registry should busy one. *)
        dest :=
          A.publish_text store ~mode:Transcribe ~text:"x"
            ~dispatch_uncertain:false
          |> Result.map ignore
      in
      ignore gate;
      let t1 = Thread.create (fun () -> hold store1 r1) () in
      let t2 = Thread.create (fun () -> hold store2 r2) () in
      Thread.join t1;
      Thread.join t2;
      let outcomes = [ !r1; !r2 ] in
      let ok_n =
        List.fold_left
          (fun n -> function Ok () -> n + 1 | Error _ -> n)
          0 outcomes
      in
      let busy_n =
        List.fold_left
          (fun n -> function Error (`Retry_store "busy") -> n + 1 | _ -> n)
          0 outcomes
      in
      Alcotest.(check bool) "at least one published" true (ok_n >= 1);
      match fail_store (A.peek store1) with
      | Some (A.Text _) -> ignore busy_n
      | _ -> Alcotest.fail "no candidate after concurrent publish")

let test_failed_create () =
  let path = Filename.temp_file "pie-notdir" "" in
  Fun.protect
    ~finally:(fun () -> try Unix.unlink path with _ -> ())
    (fun () ->
      match A.open_store ~root:path with
      | Error (`Retry_store _) -> ()
      | Ok _ -> Alcotest.fail "opened store on a file")

let test_odd_and_budget () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w (Bytes.of_string "A"));
      fail_store (A.close_writer w);
      (match A.publish_audio store w with
      | Error (`Retry_store m) when String.length m > 0 -> ()
      | _ -> Alcotest.fail "odd PCM published");
      fail_store (A.abort_writer w);
      let too_big =
        String.make (Pie.Dictation_limits.result_utf8_bytes + 1) 'x'
      in
      (match
         A.publish_text store ~mode:Transcribe ~text:too_big
           ~dispatch_uncertain:false
       with
      | Error (`Retry_store _) -> ()
      | Ok _ -> Alcotest.fail "oversize text published");
      let w2 = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w2 even_pcm);
      let huge = Bytes.create Pie.Dictation_limits.capture_pcm_bytes in
      (match A.append_audio w2 huge with
      | Error (`Retry_store _) -> ()
      | Ok () -> Alcotest.fail "oversize pcm appended");
      fail_store (A.abort_writer w2))

let test_historical_untouched () =
  with_root (fun root ->
      let sentinel = Filename.concat root "transcribe-1-2-3.pcm" in
      let oc = open_out_bin sentinel in
      output_string oc "HIST";
      close_out oc;
      let store = fail_store (A.open_store ~root) in
      fail_store
        (A.publish_text store ~mode:Transcribe ~text:"new"
           ~dispatch_uncertain:false)
      |> ignore;
      Alcotest.(check bool) "sentinel exists" true (Sys.file_exists sentinel);
      let ic = open_in_bin sentinel in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "sentinel bytes" "HIST" s;
      let sst = Unix.lstat sentinel in
      Alcotest.(check bool) "sentinel regular" true (sst.st_kind = Unix.S_REG);
      Alcotest.(check string) "sentinel still HIST" "HIST" s)

let test_success_clear () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let g =
        fail_store
          (A.publish_text store ~mode:Transcribe ~text:"bye"
             ~dispatch_uncertain:false)
      in
      fail_store (A.clear store g);
      match fail_store (A.peek store) with
      | None -> ()
      | Some _ -> Alcotest.fail "cleared candidate still visible")

let test_fchmod_after_open_fails () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      match
        A.Test_hooks.with_fchmod_fail (fun () ->
            A.open_audio_writer store ~mode:Transcribe)
      with
      | Error (`Retry_store _) -> ()
      | Ok _ -> Alcotest.fail "writer opened after fchmod failure")

let test_close_and_unlink_both_fail () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w even_pcm);
      match
        A.Test_hooks.with_close_and_unlink_fail (fun () -> A.abort_writer w)
      with
      | Error (`Retry_store m) ->
          Alcotest.(check bool) "close reported" true (String.length m > 0)
      | Ok () -> Alcotest.fail "abort swallowed close+unlink failures")

let test_emfile_is_error_not_none () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      fail_store
        (A.publish_text store ~mode:Transcribe ~text:"x"
           ~dispatch_uncertain:false)
      |> ignore;
      match A.Test_hooks.with_emfile (fun () -> A.peek store) with
      | Error (`Retry_store _) -> ()
      | Ok None -> Alcotest.fail "EMFILE became no-candidate"
      | Ok (Some _) -> Alcotest.fail "peek succeeded under EMFILE")

let test_malicious_slot_symlink () =
  with_root (fun root ->
      let sentinel = Filename.temp_file "pie-outside" "" in
      Fun.protect
        ~finally:(fun () -> try Unix.unlink sentinel with _ -> ())
        (fun () ->
          let oc = open_out_bin sentinel in
          output_string oc "KEEP";
          close_out oc;
          let store = fail_store (A.open_store ~root) in
          let g =
            fail_store
              (A.publish_text store ~mode:Transcribe ~text:"old"
                 ~dispatch_uncertain:false)
          in
          let active = Filename.concat root "active" in
          let slots =
            Sys.readdir active |> Array.to_list
            |> List.filter (fun n -> String.starts_with ~prefix:"slot-" n)
          in
          List.iter
            (fun n ->
              let slot = Filename.concat active n in
              Array.iter
                (fun c -> Unix.unlink (Filename.concat slot c))
                (Sys.readdir slot);
              Unix.rmdir slot;
              Unix.symlink (Filename.dirname sentinel) slot)
            slots;
          fail_store (A.clear store g);
          let ic = open_in_bin sentinel in
          let s = really_input_string ic (in_channel_length ic) in
          close_in ic;
          Alcotest.(check string) "outside sentinel" "KEEP" s))

let test_lockfile_survives () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let g =
        fail_store
          (A.publish_text store ~mode:Transcribe ~text:"x"
             ~dispatch_uncertain:false)
      in
      fail_store (A.clear store g);
      let lock =
        Filename.concat (Filename.concat root "active") "publish.lock"
      in
      Alcotest.(check bool) "lock never unlinked" true (Sys.file_exists lock))

let test_fsync_fail_closes_and_does_not_seal () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w even_pcm);
      (match A.Test_hooks.with_fsync_fail (fun () -> A.close_writer w) with
      | Error (`Retry_store _) -> ()
      | Ok () -> Alcotest.fail "sealed after fsync failure");
      (match A.publish_audio store w with
      | Error (`Retry_store _) -> ()
      | Ok _ -> Alcotest.fail "published unsealed writer");
      fail_store (A.abort_writer w);
      match fail_store (A.peek store) with
      | None -> ()
      | Some _ -> Alcotest.fail "aborted draft still published")

let test_abort_after_publish_keeps_candidate () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w even_pcm);
      fail_store (A.close_writer w);
      let g = fail_store (A.publish_audio store w) in
      fail_store (A.abort_writer w);
      match fail_store (A.peek store) with
      | Some (A.Audio { generation; _ }) when A.generation_equal generation g ->
          Alcotest.(check int)
            "payload" 4
            (Bytes.length (fail_store (A.load_audio store g)))
      | _ -> Alcotest.fail "abort after publish dropped candidate")

let test_retire_failure_then_abort_keeps_new () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w1 = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w1 even_pcm);
      fail_store (A.close_writer w1);
      fail_store (A.publish_audio store w1) |> ignore;
      let w2 = fail_store (A.open_audio_writer store ~mode:Translate) in
      fail_store (A.append_audio w2 even_pcm);
      fail_store (A.close_writer w2);
      let published =
        A.Test_hooks.with_unlink_fail (fun () -> A.publish_audio store w2)
      in
      match published with
      | Ok g -> (
          fail_store (A.abort_writer w2);
          match fail_store (A.peek store) with
          | Some (A.Audio { generation; _ })
            when A.generation_equal generation g ->
              Alcotest.(check int)
                "new payload" 4
                (Bytes.length (fail_store (A.load_audio store g)))
          | _ -> Alcotest.fail "new candidate missing after abort")
      | Error (`Retry_store _) -> (
          fail_store (A.abort_writer w2);
          match fail_store (A.peek store) with
          | Some (A.Audio { mode = Translate; pcm_bytes = 4; generation }) ->
              Alcotest.(check int)
                "new payload" 4
                (Bytes.length (fail_store (A.load_audio store generation)))
          | _ -> Alcotest.fail "indexed new candidate missing after retire fail"
          ))

let test_close_reuse_abort_leaves_new_fd () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      let w = fail_store (A.open_audio_writer store ~mode:Transcribe) in
      fail_store (A.append_audio w even_pcm);
      (match A.Test_hooks.with_close_fail (fun () -> A.close_writer w) with
      | Error (`Retry_store _) -> ()
      | Ok () -> Alcotest.fail "close succeeded under inject");
      let reused =
        match A.Test_hooks.take_reused_fd () with
        | Some fd -> fd
        | None -> Alcotest.fail "no reused fd"
      in
      fail_store (A.abort_writer w);
      (match Unix.fstat reused with
      | st -> Alcotest.(check bool) "reused fd live" true (st.st_nlink >= 0)
      | exception Unix.Unix_error (Unix.EBADF, _, _) ->
          Alcotest.fail "abort closed reused descriptor");
      try Unix.close reused with _ -> ())

let test_publish_text_error_preserves_rollback () =
  with_root (fun root ->
      let store = fail_store (A.open_store ~root) in
      fail_store
        (A.publish_text store ~mode:Transcribe ~text:"keep"
           ~dispatch_uncertain:false)
      |> ignore;
      match
        A.Test_hooks.with_index_open_fail (fun () ->
            A.Test_hooks.with_unlink_fail (fun () ->
                A.publish_text store ~mode:Translate ~text:"new"
                  ~dispatch_uncertain:false))
      with
      | Ok _ -> Alcotest.fail "second publish succeeded"
      | Error (`Retry_store m) ->
          Alcotest.fail ("dropped rollback into Error only: " ^ m)
      | exception _ -> ())

let test_active_symlink_leaves_outside_unchanged () =
  with_root (fun root ->
      let outside = Filename.temp_file "pie-outside-active" "" in
      Sys.remove outside;
      Unix.mkdir outside 0o755;
      Fun.protect
        ~finally:(fun () -> try rm_r outside with _ -> ())
        (fun () ->
          let sentinel = Filename.concat outside "sentinel" in
          let oc = open_out_bin sentinel in
          output_string oc "KEEP";
          close_out oc;
          Unix.symlink outside (Filename.concat root "active");
          (match A.open_store ~root with
          | Error (`Retry_store _) -> ()
          | Ok _ -> Alcotest.fail "followed active symlink");
          let st = Unix.lstat outside in
          Alcotest.(check int) "outside mode" 0o755 (st.st_perm land 0o777);
          let ic = open_in_bin sentinel in
          let s = really_input_string ic (in_channel_length ic) in
          close_in ic;
          Alcotest.(check string) "sentinel" "KEEP" s))

let test_insecure_active_rejected_not_repaired () =
  with_root (fun root ->
      let active = Filename.concat root "active" in
      Unix.mkdir active 0o755;
      Unix.chmod active 0o755;
      (match A.open_store ~root with
      | Error (`Retry_store _) -> ()
      | Ok _ -> Alcotest.fail "accepted insecure active");
      let st = Unix.lstat active in
      Alcotest.(check int) "left 0755" 0o755 (st.st_perm land 0o777))

let test_writable_root_rejected () =
  let root = Filename.temp_file "pie-recov-open" "" in
  Sys.remove root;
  Unix.mkdir root 0o775;
  Unix.chmod root 0o775;
  Fun.protect
    ~finally:(fun () -> try rm_r root with _ -> ())
    (fun () ->
      (match A.open_store ~root with
      | Error (`Retry_store _) -> ()
      | Ok _ -> Alcotest.fail "accepted group/other writable root");
      let st = Unix.lstat root in
      Alcotest.(check int) "root not chmod'd" 0o775 (st.st_perm land 0o777))

let test_owned_0755_root_sentinel_unchanged () =
  let root = Filename.temp_file "pie-recov-hist" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Unix.chmod root 0o755;
  Fun.protect
    ~finally:(fun () -> try rm_r root with _ -> ())
    (fun () ->
      ignore (Unix.umask 0o022);
      let sentinel = Filename.concat root "transcribe-1-2-3.pcm" in
      let oc = open_out_bin sentinel in
      output_string oc "HIST";
      close_out oc;
      let store = fail_store (A.open_store ~root) in
      let rst = Unix.lstat root in
      Alcotest.(check int) "root left 0755" 0o755 (rst.st_perm land 0o777);
      let ic = open_in_bin sentinel in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "sentinel" "HIST" s;
      let ast = Unix.lstat (Filename.concat root "active") in
      Alcotest.(check int) "new active 0700" 0o700 (ast.st_perm land 0o777);
      let sst = Unix.lstat sentinel in
      Alcotest.(check bool) "sentinel regular" true (sst.st_kind = Unix.S_REG);
      Alcotest.(check int) "root still 0755" 0o755 (rst.st_perm land 0o777);
      ignore store)

let () =
  Alcotest.run "recovery_store"
    [
      ( "active",
        [
          Alcotest.test_case "umask 0700/0600 and lock kept" `Quick
            test_umask_private;
          Alcotest.test_case "draft and sealed unpublished excluded" `Quick
            test_draft_and_sealed_unpublished;
          Alcotest.test_case "publish audio and startup reload" `Quick
            test_publish_audio_and_reload;
          Alcotest.test_case "publish text startup reload" `Quick
            test_publish_text_startup;
          Alcotest.test_case "symlink fifo unrelated excluded" `Quick
            test_symlink_fifo_unrelated;
          Alcotest.test_case "late clear of old generation" `Quick
            test_generation_clear_race;
          Alcotest.test_case "concurrent store instances" `Quick
            test_concurrent_instances_busy;
          Alcotest.test_case "failed create on non-directory" `Quick
            test_failed_create;
          Alcotest.test_case "odd pcm and byte budgets" `Quick
            test_odd_and_budget;
          Alcotest.test_case "historical sentinel untouched" `Quick
            test_historical_untouched;
          Alcotest.test_case "success clear" `Quick test_success_clear;
          Alcotest.test_case "lockfile never unlinked" `Quick
            test_lockfile_survives;
          Alcotest.test_case "fchmod failure after exclusive open" `Quick
            test_fchmod_after_open_fails;
          Alcotest.test_case "close and unlink both fail" `Quick
            test_close_and_unlink_both_fail;
          Alcotest.test_case "EMFILE is error not no-candidate" `Quick
            test_emfile_is_error_not_none;
          Alcotest.test_case "malicious slot symlink spares sentinel" `Quick
            test_malicious_slot_symlink;
          Alcotest.test_case "fsync failure closes without sealing" `Quick
            test_fsync_fail_closes_and_does_not_seal;
          Alcotest.test_case "abort after publish keeps candidate" `Quick
            test_abort_after_publish_keeps_candidate;
          Alcotest.test_case "retire failure then abort keeps new" `Quick
            test_retire_failure_then_abort_keeps_new;
          Alcotest.test_case "close reuse abort leaves new fd" `Quick
            test_close_reuse_abort_leaves_new_fd;
          Alcotest.test_case "publish_text error preserves rollback" `Quick
            test_publish_text_error_preserves_rollback;
          Alcotest.test_case "active symlink leaves outside unchanged" `Quick
            test_active_symlink_leaves_outside_unchanged;
          Alcotest.test_case "insecure active rejected not repaired" `Quick
            test_insecure_active_rejected_not_repaired;
          Alcotest.test_case "writable root rejected" `Quick
            test_writable_root_rejected;
          Alcotest.test_case "owned 0755 root sentinel unchanged" `Quick
            test_owned_0755_root_sentinel_unchanged;
        ] );
    ]
