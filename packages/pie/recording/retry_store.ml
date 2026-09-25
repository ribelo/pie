type mode = Transcribe | Translate

let string_of_mode = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

module Active = struct
  type generation = string

  let generation_equal = String.equal

  type store = { root : string; active : string }

  type phase = Draft | Sealed | Published

  type writer = {
    store : store;
    mode : mode;
    generation : generation;
    slot : string;
    draft : string;
    mutable fd : Unix.file_descr option;
    mutable byte_count : int;
    mutable phase : phase;
  }

  type metadata =
    | Audio of { mode : mode; pcm_bytes : int; generation : generation }
    | Text of {
        mode : mode;
        utf8_bytes : int;
        dispatch_uncertain : bool;
        generation : generation;
      }

  let err (m : string) = Error (`Retry_store m)
  let euid = Unix.geteuid

  let json_int = function
    | `Int i -> Some i
    | `Intlit s -> int_of_string_opt s
    | _ -> None

  let max_manifest_bytes = 8192
  let max_index_bytes = 1024
  let max_gen_len = 128
  let gen_counter = Atomic.make 0

  let inject_fchmod = ref false
  let inject_close = ref false
  let inject_unlink = ref false
  let inject_emfile = ref false
  let inject_index_open = ref false
  let inject_fsync = ref false

  let reused_fd : Unix.file_descr option ref = ref None

  let unix_close fd =
    if !inject_close then (
      Unix.close fd;
      (match !reused_fd with
      | Some _ -> ()
      | None ->
          reused_fd :=
            Some
              (Unix.openfile "/dev/null"
                 [ Unix.O_RDONLY; Unix.O_CLOEXEC ]
                 0));
      raise (Unix.Unix_error (Unix.EIO, "close", "")))
    else Unix.close fd

  let unix_unlink path =
    if !inject_unlink then raise (Unix.Unix_error (Unix.EIO, "unlink", ""));
    Unix.unlink path

  let unix_fchmod fd mode =
    if !inject_fchmod then raise (Unix.Unix_error (Unix.EPERM, "fchmod", ""));
    Unix.fchmod fd mode

  let unix_openfile path flags perm =
    if !inject_emfile then
      raise (Unix.Unix_error (Unix.EMFILE, "open", path));
    if !inject_index_open && Filename.basename path = "index.json" then
      raise (Unix.Unix_error (Unix.EMFILE, "open", path));
    Unix.openfile path flags perm

  let unix_fsync fd =
    if !inject_fsync then raise (Unix.Unix_error (Unix.EIO, "fsync", ""));
    Unix.fsync fd

  exception Multiple of exn * exn
  exception Result_error of [ `Retry_store of string ]

  let combine primary cleanup = Multiple (primary, cleanup)

  let known_unix = function
    | Unix.Unix_error (e, _, _) -> err (Unix.error_message e)
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> raise exn

  type collected = Unix_named of string * Unix.error | Exn of exn

  let duty acc name f =
    try
      f ();
      acc
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> acc
    | Unix.Unix_error (e, _, _) -> Unix_named (name, e) :: acc
    | exn -> Exn exn :: acc

  let collected_exn acc =
    List.fold_left
      (fun acc item ->
        let e =
          match item with
          | Unix_named (n, err) -> Unix.Unix_error (err, n, "")
          | Exn e -> e
        in
        match acc with None -> Some e | Some p -> Some (combine p e))
      None (List.rev acc)

  let only_unix acc =
    List.for_all (function Unix_named _ -> true | Exn _ -> false) acc

  let duties_done acc =
    match collected_exn acc with
    | None -> Ok ()
    | Some e when only_unix acc ->
        err
          (String.concat "; "
             (List.rev acc
             |> List.filter_map (function
                  | Unix_named (n, er) ->
                      Some (n ^ ": " ^ Unix.error_message er)
                  | Exn _ -> None)))
    | Some e -> raise e

  let fresh_generation () =
    Printf.sprintf "%d-%d-%Ld" (Unix.getpid ())
      (Atomic.fetch_and_add gen_counter 1)
      (Int64.of_float (Unix.gettimeofday () *. 1_000_000.0))

  let safe_generation g =
    let n = String.length g in
    n > 0 && n <= max_gen_len
    &&
    let rec loop i =
      i >= n
      ||
      let c = g.[i] in
      ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || c = '-')
      && loop (i + 1)
    in
    loop 0

  let mode_to_s = string_of_mode

  let mode_of_s = function
    | "transcribe" -> Some Transcribe
    | "translate" -> Some Translate
    | _ -> None

  let owned_dir st =
    st.Unix.st_kind = Unix.S_DIR
    && st.st_uid = euid ()
    && st.st_perm land 0o777 = 0o700

  let owned_reg_0600 st =
    st.Unix.st_kind = Unix.S_REG
    && st.st_uid = euid ()
    && st.st_perm land 0o777 = 0o600

  let accept_existing_0700 path st =
    if st.Unix.st_kind = Unix.S_LNK || not (owned_dir st) then
      raise (Unix.Unix_error (Unix.EPERM, "mkdir_0700", path))

  let mkdir_0700 path =
    match Unix.lstat path with
    | st -> accept_existing_0700 path st
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        match Unix.mkdir path 0o700 with
        | () ->
            Unix.chmod path 0o700;
            let st = Unix.lstat path in
            if st.st_kind = Unix.S_LNK || not (owned_dir st) then
              raise (Unix.Unix_error (Unix.EPERM, "mkdir_0700", path))
        | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
            accept_existing_0700 path (Unix.lstat path))

  let open_store ~root =
    match Unix.lstat root with
    | exception Unix.Unix_error (e, _, _) -> err (Unix.error_message e)
    | rst when rst.st_kind <> Unix.S_DIR -> err "recovery root is not a directory"
    | rst when rst.st_uid <> euid () -> err "recovery root is not owned"
    | rst when rst.st_perm land 0o022 <> 0 ->
        err "recovery root is group/other writable"
    | _ -> (
        let active = Filename.concat root "active" in
        try
          mkdir_0700 active;
          Ok { root; active }
        with exn -> known_unix exn)

  let slot_dir store generation =
    Filename.concat store.active ("slot-" ^ generation)

  let index_path store = Filename.concat store.active "index.json"
  let lock_path store = Filename.concat store.active "publish.lock"

  module Lock_key = struct
    type t = { parent_dev : int; parent_ino : int; basename : string }

    let equal a b =
      a.parent_dev = b.parent_dev && a.parent_ino = b.parent_ino
      && String.equal a.basename b.basename

    let hash k = Hashtbl.hash (k.parent_dev, k.parent_ino, k.basename)
  end

  module Lock_table = Hashtbl.Make (Lock_key)

  let lock_mutex = Mutex.create ()
  let lock_table : int Lock_table.t = Lock_table.create 8
  let lock_next = Atomic.make 1

  let lock_key path =
    let parent = Unix.lstat (Filename.dirname path) in
    {
      Lock_key.parent_dev = parent.st_dev;
      parent_ino = parent.st_ino;
      basename = Filename.basename path;
    }

  let claim_local key =
    Mutex.lock lock_mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock lock_mutex)
      (fun () ->
        if Lock_table.mem lock_table key then err "busy"
        else
          let token = Atomic.fetch_and_add lock_next 1 in
          Lock_table.add lock_table key token;
          Ok token)

  let release_local key token =
    Mutex.lock lock_mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock lock_mutex)
      (fun () ->
        match Lock_table.find_opt lock_table key with
        | Some current when current = token -> Lock_table.remove lock_table key
        | Some _ | None -> ())

  type held_lock = {
    lock_fd : Unix.file_descr;
    lock_key : Lock_key.t;
    lock_token : int;
  }

  let close_owned owned fd =
    if !owned then (
      owned := false;
      unix_close fd)

  (* Never unlink the lockfile. F_TLOCK is non-blocking. Identity checks are
     not a kernel nofollow guarantee. Cooperating same-UID processes.
     Process-local registry is required because POSIX lockf is per-process. *)
  let acquire_publish_lock store =
    let path = lock_path store in
    let own fd =
      let st = Unix.fstat fd in
      if
        st.st_kind <> Unix.S_REG
        || st.st_uid <> euid ()
        || st.st_perm land 0o777 land 0o077 <> 0
        || st.st_nlink <> 1
      then raise (Unix.Unix_error (Unix.EPERM, "lock_identity", path))
      else st
    in
    let open_existing st =
      if
        st.Unix.st_kind = Unix.S_LNK
        || st.st_kind <> Unix.S_REG || st.st_uid <> euid ()
        || st.st_perm land 0o777 land 0o077 <> 0 || st.st_nlink <> 1
      then raise (Unix.Unix_error (Unix.EPERM, "lock_identity", path));
      let fd = unix_openfile path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600 in
      let owned = ref true in
      try
        let st2 = own fd in
        if st.st_dev <> st2.st_dev || st.st_ino <> st2.st_ino then (
          close_owned owned fd;
          raise (Unix.Unix_error (Unix.EPERM, "lock_identity", path)))
        else (
          owned := false;
          fd)
      with exn ->
        (match close_owned owned fd with
        | () -> raise exn
        | exception close_exn -> raise (combine exn close_exn))
    in
    try
      let fd =
        match Unix.lstat path with
        | st -> open_existing st
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
            try
              let fd =
                unix_openfile path
                  [
                    Unix.O_RDWR;
                    Unix.O_CREAT;
                    Unix.O_EXCL;
                    Unix.O_CLOEXEC;
                  ]
                  0o600
              in
              let owned = ref true in
              try
                unix_fchmod fd 0o600;
                ignore (own fd);
                owned := false;
                fd
              with exn ->
                (match close_owned owned fd with
                | () -> raise exn
                | exception close_exn -> raise (combine exn close_exn))
            with Unix.Unix_error (Unix.EEXIST, _, _) ->
              open_existing (Unix.lstat path))
      in
      let key = lock_key path in
      match claim_local key with
      | Error e -> (
          match unix_close fd with
          | () -> Error e
          | exception close_exn -> raise close_exn)
      | Ok token -> (
          match Unix.lockf fd Unix.F_TLOCK 0 with
          | () -> Ok { lock_fd = fd; lock_key = key; lock_token = token }
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> (
              release_local key token;
              match unix_close fd with
              | () -> err "busy"
              | exception close_exn -> raise close_exn)
          | exception exn ->
              release_local key token;
              (match unix_close fd with
              | () -> raise exn
              | exception close_exn -> raise (combine exn close_exn)))
    with exn -> known_unix exn

  let with_publish_lock store f =
    match acquire_publish_lock store with
    | Error e -> Error e
    | Ok held ->
        let owned = ref true in
        let op_error = ref None in
        let body =
          match f () with
          | v ->
              (match v with
              | Error (`Retry_store m) -> op_error := Some m
              | _ -> ());
              Ok v
          | exception exn -> Error exn
        in
        let close_exn =
          match close_owned owned held.lock_fd with
          | () -> None
          | exception exn -> Some exn
        in
        release_local held.lock_key held.lock_token;
        match (body, close_exn) with
        | Ok v, None -> v
        | Ok _, Some cleanup -> (
            match !op_error with
            | Some m -> raise (combine (Result_error (`Retry_store m)) cleanup)
            | None -> raise cleanup)
        | Error exn, None -> raise exn
        | Error primary, Some cleanup -> raise (combine primary cleanup)

  let open_excl_0600 path =
    let fd =
      unix_openfile path
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
        0o600
    in
    let owned = ref true in
    try
      unix_fchmod fd 0o600;
      owned := false;
      fd
    with exn ->
      (match close_owned owned fd with
      | () -> raise exn
      | exception close_exn -> raise (combine exn close_exn))

  let rec collision_retry n f =
    if n <= 0 then err "could not allocate exclusive recovery slot"
    else
      match f () with
      | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
          collision_retry (n - 1) f
      | v -> v

  let open_audio_writer store ~mode =
    try
      collision_retry 32 (fun () ->
          let generation = fresh_generation () in
          if not (safe_generation generation) then
            raise (Unix.Unix_error (Unix.EINVAL, "generation", ""));
          let slot = slot_dir store generation in
          Unix.mkdir slot 0o700;
          let draft = Filename.concat slot "draft.pcm" in
          try
            Unix.chmod slot 0o700;
            let fd = open_excl_0600 draft in
            Ok
              {
                store;
                mode;
                generation;
                slot;
                draft;
                fd = Some fd;
                byte_count = 0;
                phase = Draft;
              }
          with exn ->
            let acc =
              duty [] "unlink-draft" (fun () -> unix_unlink draft)
            in
            let acc = duty acc "rmdir" (fun () -> Unix.rmdir slot) in
            match collected_exn acc with
            | None -> raise exn
            | Some c -> raise (combine exn c))
    with exn -> known_unix exn

  let append_audio (writer : writer) bytes =
    match writer.fd with
    | None -> err "audio writer is closed"
    | Some fd ->
        let n = Bytes.length bytes in
        if writer.byte_count + n > Dictation_limits.capture_pcm_bytes then
          err "PCM exceeds capture bound"
        else
          try
            let rec loop off =
              if off < n then
                let w = Unix.write fd bytes off (n - off) in
                if w = 0 then
                  raise (Unix.Unix_error (Unix.EIO, "write", writer.draft))
                else loop (off + w)
            in
            loop 0;
            writer.byte_count <- writer.byte_count + n;
            Ok ()
          with exn -> known_unix exn

  let close_writer (writer : writer) =
    match writer.phase with
    | Published -> err "already published"
    | Sealed -> Ok ()
    | Draft -> (
        match writer.fd with
        | None -> err "audio writer is closed"
        | Some fd ->
            let sync_exn =
              match unix_fsync fd with
              | () -> None
              | exception exn -> Some exn
            in
            writer.fd <- None;
            let close_exn =
              match unix_close fd with
              | () -> None
              | exception exn -> Some exn
            in
            match (sync_exn, close_exn) with
            | None, None ->
                writer.phase <- Sealed;
                Ok ()
            | Some exn, None -> known_unix exn
            | None, Some exn -> known_unix exn
            | Some a, Some b -> raise (combine a b))

  let abort_writer (writer : writer) =
    match writer.phase with
    | Published -> Ok ()
    | Draft | Sealed ->
        let acc =
          duty [] "close" (fun () ->
              match writer.fd with
              | None -> ()
              | Some fd ->
                  writer.fd <- None;
                  unix_close fd)
        in
        let acc =
          duty acc "unlink-draft" (fun () -> unix_unlink writer.draft)
        in
        let acc =
          duty acc "unlink-payload" (fun () ->
              unix_unlink (Filename.concat writer.slot "payload"))
        in
        let acc =
          duty acc "unlink-manifest" (fun () ->
              unix_unlink (Filename.concat writer.slot "manifest.json"))
        in
        let acc = duty acc "rmdir" (fun () -> Unix.rmdir writer.slot) in
        duties_done acc

  let write_all fd bytes =
    let n = Bytes.length bytes in
    let rec loop off =
      if off < n then
        let w = Unix.write fd bytes off (n - off) in
        if w = 0 then raise (Unix.Unix_error (Unix.EIO, "write", ""))
        else loop (off + w)
    in
    loop 0

  let write_file_0600 path contents =
    let fd = open_excl_0600 path in
    let owned = ref true in
    try
      write_all fd (Bytes.of_string contents);
      Unix.fsync fd;
      close_owned owned fd
    with exn ->
      (match close_owned owned fd with
      | () -> raise exn
      | exception close_exn -> raise (combine exn close_exn))

  let atomic_replace_index store json =
    let dest = index_path store in
    let rec cand n =
      if n > 32 then raise (Unix.Unix_error (Unix.EEXIST, "index", dest))
      else
        let p =
          Filename.concat store.active
            (Printf.sprintf ".index-%d-%d.json" (Unix.getpid ()) n)
        in
        match open_excl_0600 p with
        | fd -> (p, fd)
        | exception Unix.Unix_error (Unix.EEXIST, _, _) -> cand (n + 1)
    in
    let tmp, fd = cand 0 in
    let owned = ref true in
    let published = ref false in
    try
      write_all fd (Bytes.of_string json);
      Unix.fsync fd;
      close_owned owned fd;
      Unix.rename tmp dest;
      published := true
    with exn ->
      let close_exn =
        match close_owned owned fd with
        | () -> None
        | exception c -> Some c
      in
      let unlink_exn =
        if !published then None
        else
          match unix_unlink tmp with
          | () -> None
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
          | exception u -> Some u
      in
      match (close_exn, unlink_exn) with
      | None, None -> raise exn
      | Some c, None -> raise (combine exn c)
      | None, Some u -> raise (combine exn u)
      | Some c, Some u -> raise (combine exn (combine c u))

  let read_index store =
    let path = index_path store in
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
    | exception Unix.Unix_error (e, _, _) -> err (Unix.error_message e)
    | st when st.st_kind = Unix.S_LNK -> Ok None
    | st when not (owned_reg_0600 st) -> Ok None
    | st -> (
        match
          unix_openfile path
            [ Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC ]
            0
        with
        | exception Unix.Unix_error (e, _, _) -> err (Unix.error_message e)
        | fd ->
        let owned = ref true in
        try
          let st2 = Unix.fstat fd in
          if st.st_dev <> st2.st_dev || st.st_ino <> st2.st_ino then (
            close_owned owned fd;
            Ok None)
          else if st2.st_size > max_index_bytes || st2.st_size <= 0 then (
            close_owned owned fd;
            Ok None)
          else
            let buf = Bytes.create st2.st_size in
            let rec loop off =
              if off < st2.st_size then
                let r = Unix.read fd buf off (st2.st_size - off) in
                if r = 0 then off else loop (off + r)
              else off
            in
            let got = loop 0 in
            close_owned owned fd;
            if got <> st2.st_size then Ok None
            else
              match Yojson.Safe.from_string (Bytes.to_string buf) with
              | `Assoc fields -> (
                  match
                    ( List.assoc_opt "version" fields,
                      List.assoc_opt "generation" fields )
                  with
                  | Some ver, Some (`String g)
                    when json_int ver = Some 1 && safe_generation g ->
                      Ok (Some g)
                  | _ -> Ok None)
              | _ -> Ok None
              | exception Yojson.Json_error _ -> Ok None
        with exn ->
          (match close_owned owned fd with
          | () -> known_unix exn
          | exception close_exn -> raise (combine exn close_exn)))

  let retire_slot store generation acc =
    let slot = slot_dir store generation in
    match Unix.lstat slot with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> acc
    | exception Unix.Unix_error (e, _, _) -> Unix_named ("lstat-slot", e) :: acc
    | st when st.st_kind = Unix.S_LNK -> acc
    | st when not (owned_dir st) -> acc
    | _ ->
        let acc =
          List.fold_left
            (fun acc name ->
              let p = Filename.concat slot name in
              match Unix.lstat p with
              | exception Unix.Unix_error (Unix.ENOENT, _, _) -> acc
              | exception Unix.Unix_error (e, _, _) -> Unix_named (name, e) :: acc
              | st when st.st_kind = Unix.S_LNK -> acc
              | st when owned_reg_0600 st ->
                  duty acc name (fun () -> unix_unlink p)
              | _ -> acc)
            acc
            [ "payload"; "manifest.json"; "draft.pcm" ]
        in
        duty acc "rmdir" (fun () -> Unix.rmdir slot)

  let rollback_unpublished_slot slot =
    let acc =
      duty [] "unlink-payload" (fun () ->
          unix_unlink (Filename.concat slot "payload"))
    in
    let acc =
      duty acc "unlink-manifest" (fun () ->
          unix_unlink (Filename.concat slot "manifest.json"))
    in
    let acc =
      duty acc "unlink-draft" (fun () ->
          unix_unlink (Filename.concat slot "draft.pcm"))
    in
    duty acc "rmdir" (fun () -> Unix.rmdir slot)

  let publish_index store generation ~on_commit =
    match read_index store with
    | Error e -> Error e
    | Ok prev -> (
        try
          let json =
            Yojson.Safe.to_string
              (`Assoc
                 [ ("version", `Int 1); ("generation", `String generation) ])
          in
          if String.length json > max_index_bytes then
            raise (Unix.Unix_error (Unix.EFBIG, "index", ""));
          atomic_replace_index store json;
          on_commit ();
          match prev with
          | Some old when old <> generation ->
              duties_done (retire_slot store old [])
          | _ -> Ok ()
        with exn -> known_unix exn)

  let write_manifest slot generation tag mode extra =
    let fields =
      [
        ("version", `Int 1);
        ("generation", `String generation);
        ("tag", `String tag);
        ("mode", `String (mode_to_s mode));
      ]
      @ extra
    in
    let json = Yojson.Safe.to_string (`Assoc fields) in
    if String.length json > max_manifest_bytes then
      raise (Unix.Unix_error (Unix.EFBIG, "manifest", slot));
    write_file_0600 (Filename.concat slot "manifest.json") json

  let publish_audio store (writer : writer) =
    with_publish_lock store (fun () ->
        if writer.store.active <> store.active then err "writer/store mismatch"
        else if writer.phase <> Sealed || writer.fd <> None then
          err "audio writer is not sealed"
        else if writer.byte_count land 1 <> 0 then err "odd PCM length"
        else if writer.byte_count <= 0 then err "empty PCM"
        else
          try
            let payload = Filename.concat writer.slot "payload" in
            Unix.rename writer.draft payload;
            write_manifest writer.slot writer.generation "audio" writer.mode
              [ ("pcm_bytes", `Int writer.byte_count) ];
            match
              publish_index store writer.generation
                ~on_commit:(fun () -> writer.phase <- Published)
            with
            | Error e -> Error e
            | Ok () -> Ok writer.generation
          with exn -> known_unix exn)

  let publish_text store ~mode ~text ~dispatch_uncertain =
    with_publish_lock store (fun () ->
        let n = String.length text in
        if n > Dictation_limits.result_utf8_bytes then
          err "text exceeds UTF-8 bound"
        else if n <= 0 then err "empty text"
        else
          collision_retry 32 (fun () ->
              let generation = fresh_generation () in
              let slot = slot_dir store generation in
              Unix.mkdir slot 0o700;
              let committed = ref false in
              try
                Unix.chmod slot 0o700;
                let payload = Filename.concat slot "payload" in
                write_file_0600 payload text;
                write_manifest slot generation "text" mode
                  [
                    ("utf8_bytes", `Int n);
                    ("dispatch_uncertain", `Bool dispatch_uncertain);
                  ];
                match
                  publish_index store generation
                    ~on_commit:(fun () -> committed := true)
                with
                | Ok () -> Ok generation
                | Error e ->
                    if !committed then Error e
                    else (
                      match duties_done (rollback_unpublished_slot slot) with
                      | Ok () -> Error e
                      | Error rollback ->
                          let of_err = function
                            | `Retry_store m -> Result_error (`Retry_store m)
                          in
                          raise (combine (of_err e) (of_err rollback)))
              with exn ->
                if not !committed then
                  (match collected_exn (rollback_unpublished_slot slot) with
                  | None -> raise exn
                  | Some c -> raise (combine exn c))
                else raise exn))

  type opened = Excluded | Opened of Unix.file_descr * Unix.stats

  let open_owned_regular path max_bytes =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok Excluded
    | exception Unix.Unix_error (e, _, _) -> err (Unix.error_message e)
    | st when st.st_kind = Unix.S_LNK -> Ok Excluded
    | st when not (owned_reg_0600 st) -> Ok Excluded
    | st when st.st_size <= 0 || st.st_size > max_bytes -> Ok Excluded
    | st ->
        let fd =
          unix_openfile path
            [ Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC ]
            0
        in
        let owned = ref true in
        try
          let st2 = Unix.fstat fd in
          if st.st_dev <> st2.st_dev || st.st_ino <> st2.st_ino then (
            close_owned owned fd;
            Ok Excluded)
          else (
            owned := false;
            Ok (Opened (fd, st2)))
        with exn ->
          (match close_owned owned fd with
          | () -> known_unix exn
          | exception close_exn -> raise (combine exn close_exn))

  let read_fd fd size =
    let buf = Bytes.create size in
    let rec loop off =
      if off < size then
        let r = Unix.read fd buf off (size - off) in
        if r = 0 then off else loop (off + r)
      else off
    in
    let got = loop 0 in
    if got <> size then None else Some buf

  let peek store =
    match read_index store with
    | Error e -> Error e
    | Ok None -> Ok None
    | Ok (Some generation) -> (
        let slot = slot_dir store generation in
        (match Unix.lstat slot with
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
        | exception Unix.Unix_error (e, _, _) -> err (Unix.error_message e)
        | st when st.st_kind = Unix.S_LNK -> Ok None
        | st when not (owned_dir st) -> Ok None
        | _ -> (
            let man = Filename.concat slot "manifest.json" in
            match open_owned_regular man max_manifest_bytes with
            | Error e -> Error e
            | Ok Excluded -> Ok None
            | Ok (Opened (fd, st)) ->
                let owned = ref true in
                try
                  let parsed =
                    match read_fd fd st.st_size with
                    | None -> Ok None
                    | Some buf -> (
                        match Yojson.Safe.from_string (Bytes.to_string buf) with
                        | `Assoc fields -> (
                            let get_s k =
                              match List.assoc_opt k fields with
                              | Some (`String s) -> Some s
                              | _ -> None
                            in
                            let get_i k =
                              match List.assoc_opt k fields with
                              | Some v -> json_int v
                              | None -> None
                            in
                            match
                              ( List.assoc_opt "version" fields,
                                get_s "generation",
                                get_s "tag",
                                get_s "mode" )
                            with
                            | Some ver, Some g, Some tag, Some ms
                              when json_int ver = Some 1
                                   && g = generation && safe_generation g -> (
                                match (tag, mode_of_s ms) with
                                | "audio", Some mode -> (
                                    match get_i "pcm_bytes" with
                                    | Some pcm_bytes when pcm_bytes > 0 ->
                                        Ok
                                          (Some
                                             (Audio
                                                { mode; pcm_bytes; generation }))
                                    | _ -> Ok None)
                                | "text", Some mode -> (
                                    match
                                      ( get_i "utf8_bytes",
                                        List.assoc_opt "dispatch_uncertain"
                                          fields )
                                    with
                                    | ( Some utf8_bytes,
                                        Some (`Bool dispatch_uncertain) )
                                      when utf8_bytes > 0 ->
                                        Ok
                                          (Some
                                             (Text
                                                {
                                                  mode;
                                                  utf8_bytes;
                                                  dispatch_uncertain;
                                                  generation;
                                                }))
                                    | _ -> Ok None)
                                | _ -> Ok None)
                            | _ -> Ok None)
                        | _ -> Ok None
                        | exception Yojson.Json_error _ -> Ok None)
                  in
                  close_owned owned fd;
                  parsed
                with exn ->
                  (match close_owned owned fd with
                  | () -> known_unix exn
                  | exception close_exn -> raise (combine exn close_exn)))))

  let load_payload store generation max_bytes =
    match peek store with
    | Error e -> Error e
    | Ok None -> err "no published candidate"
    | Ok (Some (Audio { generation = g; _ } | Text { generation = g; _ }))
      when not (generation_equal g generation) ->
        err "generation mismatch"
    | Ok (Some _) -> (
        let path = Filename.concat (slot_dir store generation) "payload" in
        match open_owned_regular path max_bytes with
        | Error e -> Error e
        | Ok Excluded -> err "payload rejected"
        | Ok (Opened (fd, st)) ->
            let owned = ref true in
            try
              let body =
                match read_fd fd st.st_size with
                | None -> err "payload read failed"
                | Some b -> Ok b
              in
              close_owned owned fd;
              body
            with exn ->
              (match close_owned owned fd with
              | () -> known_unix exn
              | exception close_exn -> raise (combine exn close_exn)))

  let load_audio store generation =
    match peek store with
    | Error e -> Error e
    | Ok (Some (Audio { generation = g; pcm_bytes; _ }))
      when generation_equal g generation -> (
        match load_payload store generation Dictation_limits.capture_pcm_bytes with
        | Error e -> Error e
        | Ok b when Bytes.length b <> pcm_bytes -> err "pcm length mismatch"
        | Ok b -> Ok b)
    | Ok _ -> err "published candidate is not audio"

  let load_text store generation =
    match peek store with
    | Error e -> Error e
    | Ok (Some (Text { generation = g; utf8_bytes; _ }))
      when generation_equal g generation -> (
        match load_payload store generation Dictation_limits.result_utf8_bytes with
        | Error e -> Error e
        | Ok b when Bytes.length b <> utf8_bytes -> err "utf8 length mismatch"
        | Ok b -> Ok (Bytes.to_string b))
    | Ok _ -> err "published candidate is not text"

  let clear store generation =
    with_publish_lock store (fun () ->
        match read_index store with
        | Error e -> Error e
        | Ok (Some g) when generation_equal g generation ->
            let acc =
              duty [] "unlink-index" (fun () -> unix_unlink (index_path store))
            in
            duties_done (retire_slot store generation acc)
        | Ok _ -> Ok ())

  module Test_hooks = struct
    let with_flag flag f =
      let prev = !flag in
      flag := true;
      match f () with
      | v ->
          flag := prev;
          v
      | exception exn ->
          flag := prev;
          raise exn

    let with_fchmod_fail f = with_flag inject_fchmod f

    let with_close_fail f =
      reused_fd := None;
      with_flag inject_close f

    let take_reused_fd () =
      let fd = !reused_fd in
      reused_fd := None;
      fd
    let with_unlink_fail f = with_flag inject_unlink f
    let with_emfile f = with_flag inject_emfile f
    let with_index_open_fail f = with_flag inject_index_open f
    let with_fsync_fail f = with_flag inject_fsync f

    let with_close_and_unlink_fail f =
      let pc = !inject_close and pu = !inject_unlink in
      inject_close := true;
      inject_unlink := true;
      match f () with
      | v ->
          inject_close := pc;
          inject_unlink := pu;
          v
      | exception exn ->
          inject_close := pc;
          inject_unlink := pu;
          raise exn
  end
end
