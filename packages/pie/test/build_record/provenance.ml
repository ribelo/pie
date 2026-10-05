(* Build-interval provenance for pie.exe. *)

let contains needle hay =
  let n = String.length needle in
  let h = String.length hay in
  let rec loop i =
    i <= h - n && (String.sub hay i n = needle || loop (i + 1))
  in
  loop 0

let read_process ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) cmd =
  let ic = Ox_caps.Cap_unix.open_process_in ~caps cmd in
  Fun.protect
    ~finally:(fun () -> ignore (Unix.close_process_in ic))
    (fun () ->
      let b = Buffer.create 256 in
      (try
         while true do
           Buffer.add_string b (input_line ic);
           Buffer.add_char b '\n'
         done
       with End_of_file -> ());
      String.trim (Buffer.contents b))

let sha256_file ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) path =
  let out = read_process ~caps (Filename.quote_command "sha256sum" [ path ]) in
  match String.split_on_char ' ' out with
  | hash :: _ -> hash
  | [] -> failwith ("sha256sum failed: " ^ path)

let embedded_build_identity ~(caps : < Ox_caps.Cap.fs_read ; .. >) build_dir =
  let path =
    Filename.concat build_dir "default/packages/pie/build_identity.ml"
  in
  if not (Sys.file_exists path) then `Null
  else
    let channel = Ox_caps.Cap_stdlib.open_in ~caps path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let commit = ref None and time = ref None in
        (try
           while true do
             let line = input_line channel in
             if String.starts_with ~prefix:"let commit = " line then
               commit := Some (Scanf.sscanf line "let commit = %S" Fun.id)
             else if String.starts_with ~prefix:"let time = " line then
               time := Some (Scanf.sscanf line "let time = %S" Fun.id)
           done
         with End_of_file -> ());
        match (!commit, !time) with
        | Some commit, Some time ->
            `Assoc [ ("commit", `String commit); ("time", `String time) ]
        | _ -> failwith "generated build identity is incomplete")

let ocaml_version ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) () =
  read_process ~caps "ocaml -version"

let dune_version ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) () =
  read_process ~caps "dune --version"

let git_head ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) root =
  if Sys.file_exists (Filename.concat root ".git") then
    read_process ~caps
      (Filename.quote_command "git" [ "-C"; root; "rev-parse"; "HEAD" ])
  else "unknown"

let git_dirty ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) root =
  if not (Sys.file_exists (Filename.concat root ".git")) then true
  else
    let st =
      read_process ~caps
        (Filename.quote_command "git" [ "-C"; root; "status"; "--porcelain" ])
    in
    st <> ""

let rec collect_files ~(caps : < Ox_caps.Cap.fs_read ; .. >) acc dir =
  match Ox_caps.Cap_unix.opendir ~caps dir with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> acc
  | dh ->
      Fun.protect
        ~finally:(fun () -> Unix.closedir dh)
        (fun () ->
          let rec loop acc =
            match Unix.readdir dh with
            | exception End_of_file -> acc
            | "." | ".." -> loop acc
            | name -> (
                let path = Filename.concat dir name in
                match Unix.lstat path with
                | { Unix.st_kind = Unix.S_DIR; _ } ->
                    if String.length name > 0 && name.[0] = '.' then loop acc
                    else loop (collect_files ~caps acc path)
                | { Unix.st_kind = Unix.S_REG; _ } -> loop (path :: acc)
                | _ -> loop acc)
          in
          loop acc)

let relative_to root path =
  let root =
    if String.length root > 0 && root.[String.length root - 1] = '/' then root
    else root ^ "/"
  in
  let n = String.length root in
  if String.length path >= n && String.sub path 0 n = root then
    String.sub path n (String.length path - n)
  else path

let fingerprint_files
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) root paths =
  let rels =
    paths
    |> List.map (fun p -> (relative_to root p, p))
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let lines =
    List.map (fun (rel, abs) -> sha256_file ~caps abs ^ "  " ^ rel) rels
  in
  let tmp = Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-fp" "" in
  Fun.protect
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Ox_caps.Cap_unix.unlink ~caps tmp)
        with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
    (fun () ->
      let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps tmp in
      List.iter
        (fun line ->
          output_string oc line;
          output_char oc '\n')
        lines;
      close_out oc;
      sha256_file ~caps tmp)

let config_inputs root =
  [
    "dune-project";
    "scripts/build-identity.sh";
    "dune-workspace";
    "packages/pie/dune";
    "packages/pie/bin/dune";
  ]
  |> List.filter_map (fun n ->
      let p = Filename.concat root n in
      if Sys.file_exists p then Some p else None)

let source_paths ~(caps : < Ox_caps.Cap.fs_read ; .. >) root =
  collect_files ~caps [] (Filename.concat root "packages/pie")
  @ config_inputs root

let source_fingerprint
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) root =
  fingerprint_files ~caps root (source_paths ~caps root)

let ocamlpath_dirs ~(caps : < Ox_caps.Cap.env ; .. >) () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "OCAMLPATH" with
  | None | Some "" -> []
  | Some p -> String.split_on_char ':' p |> List.filter (( <> ) "")

let caml_ld_dirs ~(caps : < Ox_caps.Cap.env ; .. >) () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "CAML_LD_LIBRARY_PATH" with
  | None | Some "" -> []
  | Some p -> String.split_on_char ':' p |> List.filter (( <> ) "")

let find_ox_cmxa_env ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; .. >) ()
    =
  ocamlpath_dirs ~caps ()
  |> List.concat_map (fun dir ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Ox_caps.Cap_sys.readdir ~caps dir)
        with exn -> Result.Error exn
      with
      | Result.Error _ -> []
      | Result.Ok names ->
          Array.to_list names
          |> List.filter_map (fun name ->
              let lower = String.lowercase_ascii name in
              if String.length lower < 2 || String.sub lower 0 2 <> "ox" then
                None
              else
                let p =
                  Filename.concat (Filename.concat dir name) (name ^ ".cmxa")
                in
                if Sys.file_exists p then Some p else None))

let find_tls_stub ~(caps : < Ox_caps.Cap.env ; .. >) () =
  caml_ld_dirs ~caps ()
  |> List.find_map (fun dir ->
      let stub = Filename.concat dir "dllox_http_stubs.so" in
      if Sys.file_exists stub then Some stub else None)

let hash_map ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) paths =
  paths
  |> List.map (fun p -> (p, sha256_file ~caps p))
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let tokenize_command line =
  let len = String.length line in
  let rec skip i =
    if i < len && (line.[i] = ' ' || line.[i] = '\t') then skip (i + 1) else i
  in
  let rec tok i acc =
    let i = skip i in
    if i >= len then List.rev acc
    else if line.[i] = '\\' then
      failwith "unsupported backslash quoting in link command"
    else if line.[i] = '\'' then
      failwith "unsupported single-quote quoting in link command"
    else if line.[i] = '"' then
      let rec quoted j =
        if j >= len then failwith "unterminated double quote in link command"
        else if line.[j] = '\\' then
          failwith "unsupported backslash quoting in link command"
        else if line.[j] = '"' then
          tok (j + 1) (String.sub line (i + 1) (j - i - 1) :: acc)
        else quoted (j + 1)
      in
      quoted (i + 1)
    else
      let rec bare j =
        if j >= len then j
        else
          match line.[j] with
          | ' ' | '\t' | '"' | '\'' | '\\' -> j
          | _ -> bare (j + 1)
      in
      let j = bare i in
      tok j (String.sub line i (j - i) :: acc)
  in
  tok 0 []

let resolve_path ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let parse_pie_link_cmxa ~expected_output ~root ~build_dir log_text =
  let resolve out =
    [
      resolve_path ~cwd:root out;
      resolve_path ~cwd:build_dir out;
      resolve_path ~cwd:(Filename.concat build_dir "default") out;
    ]
  in
  let matches_output out = List.mem expected_output (resolve out) in
  let lines = String.split_on_char '\n' log_text in
  let candidates =
    List.filter_map
      (fun line ->
        if not (contains "ocamlopt" line && contains ".cmxa" line) then None
        else
          match tokenize_command line with
          | exception Failure _ -> None
          | argv ->
              let rec find_o = function
                | "-o" :: out :: _ when matches_output out -> Some argv
                | _ :: rest -> find_o rest
                | [] -> None
              in
              find_o argv)
      lines
  in
  match candidates with
  | [] -> failwith "no ocamlopt link line for the expected pie.exe output"
  | [ argv ] -> List.filter (fun tok -> Filename.check_suffix tok ".cmxa") argv
  | _ -> failwith "multiple ocamlopt link lines for the expected pie.exe output"

let ox_linked paths =
  List.filter
    (fun p ->
      let base = String.lowercase_ascii (Filename.basename p) in
      String.length base >= 2 && String.sub base 0 2 = "ox")
    paths

let companion_archive cmxa =
  if not (Filename.check_suffix cmxa ".cmxa") then
    failwith ("not a cmxa: " ^ cmxa);
  let archive = Filename.chop_suffix cmxa ".cmxa" ^ ".a" in
  if not (Sys.file_exists archive) then
    failwith ("missing companion archive: " ^ archive);
  archive

let stub_archives_in ~(caps : < Ox_caps.Cap.fs_read ; .. >) dir =
  (* Preserve the exception as data before this test classifies its outcome. *)
  match
    try Result.Ok (Ox_caps.Cap_sys.readdir ~caps dir)
    with exn -> Result.Error exn
  with
  | Result.Error _ -> []
  | Result.Ok names ->
      Array.to_list names
      |> List.filter_map (fun name ->
          let lower = String.lowercase_ascii name in
          if Filename.check_suffix lower ".a" && contains "stubs" lower then
            Some (Filename.concat dir name)
          else None)

let package_stub_archives ~(caps : < Ox_caps.Cap.fs_read ; .. >) dir =
  match stub_archives_in ~caps dir with
  | [] -> []
  | [ path ] -> [ path ]
  | many ->
      failwith
        ("ambiguous native C stub archives in " ^ dir ^ ": "
       ^ String.concat ", " many)

let package_native_inputs ~(caps : < Ox_caps.Cap.fs_read ; .. >) cmxa =
  companion_archive cmxa :: package_stub_archives ~caps (Filename.dirname cmxa)

let visible_native_inputs ~(caps : < Ox_caps.Cap.fs_read ; .. >) cmxa_paths =
  List.concat_map
    (fun cmxa -> cmxa :: package_native_inputs ~caps cmxa)
    cmxa_paths

let known_tls_cmxa_name = "ox_http.cmxa"
let known_tls_stub_name = "libox_http_stubs.a"

let linked_tls_stub_archive cmxa_paths =
  let tls_cmxas =
    cmxa_paths
    |> List.filter (fun p -> Filename.basename p = known_tls_cmxa_name)
    |> List.sort_uniq String.compare
  in
  match tls_cmxas with
  | [] -> None
  | [ cmxa ] ->
      let stub = Filename.concat (Filename.dirname cmxa) known_tls_stub_name in
      if not (Sys.file_exists stub) then
        failwith ("missing TLS native C stub archive: " ^ stub)
      else Some stub
  | many ->
      failwith
        ("duplicate linked ox_http.cmxa versions: " ^ String.concat ", " many)

type file_id = { name : string; path : string; sha256 : string }

let id_of_path ~(caps : < Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) path =
  { name = Filename.basename path; path; sha256 = sha256_file ~caps path }

let file_id_json e : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String e.name);
      ("path", `String e.path);
      ("sha256", `String e.sha256);
    ]

let unique_candidate ~(caps : < Ox_caps.Cap.fs_write ; .. >) dir =
  let rec loop n =
    if n > 1024 then failwith "could not allocate provenance candidate"
    else
      let path =
        Filename.concat dir
          (Printf.sprintf ".pie-prov-%d-%d.json" (Unix.getpid ()) n)
      in
      match
        Ox_caps.Cap_unix.openfile_write ~caps path
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]
          0o600
      with
      | fd -> (path, fd)
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n + 1)
  in
  loop 0

let atomic_write ~(caps : < Ox_caps.Cap.fs_write ; .. >) path contents =
  let dir = Filename.dirname path in
  let cand, fd = unique_candidate ~caps dir in
  let oc = Unix.out_channel_of_descr fd in
  let published = ref false in
  Fun.protect
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      (match
         try Result.Ok (close_out_noerr oc) with exn -> Result.Error exn
       with
      | Result.Ok value -> value
      | Result.Error _ -> ());
      if not !published then
        (* Preserve the exception as data before this test classifies its outcome. *)
        match
          try Result.Ok (Ox_caps.Cap_unix.unlink ~caps cand)
          with exn -> Result.Error exn
        with
        | Result.Ok value -> value
        | Result.Error _ -> ())
    (fun () ->
      output_string oc contents;
      flush oc;
      close_out oc;
      Ox_caps.Cap_unix.rename ~caps cand path;
      published := true)

let default_dune_build
    ~(caps : < Ox_caps.Cap.chdir ; Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >)
    ~root ~build_dir ~target ~verbose_log () =
  let old = Unix.getcwd () in
  Ox_caps.Cap_unix.chdir ~caps root;
  Fun.protect
    ~finally:(fun () -> Ox_caps.Cap_unix.chdir ~caps old)
    (fun () ->
      let cmd =
        Filename.quote_command "dune"
          [
            "build";
            "--verbose";
            "--root";
            root;
            "--build-dir";
            build_dir;
            target;
          ]
      in
      let redirected =
        "DUNE_CACHE=disabled " ^ cmd ^ " > " ^ Filename.quote verbose_log
        ^ " 2>&1"
      in
      match Ox_caps.Cap_unix.system ~caps redirected with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n -> failwith ("dune build exited " ^ string_of_int n)
      | _ -> failwith "dune build failed")

let write_record
    ~(caps :
       < Ox_caps.Cap.chdir
       ; Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) ~root ~build_dir ~target ~output ?run_build () =
  let source_before = source_fingerprint ~caps root in
  let ox_cmxa_env = find_ox_cmxa_env ~caps () in
  let ox_env_before =
    hash_map ~caps (visible_native_inputs ~caps ox_cmxa_env)
  in
  let stub_before =
    match find_tls_stub ~caps () with
    | None -> None
    | Some p -> Some (p, sha256_file ~caps p)
  in
  let binary_path =
    Filename.concat build_dir (Filename.concat "default" target)
  in
  (match run_build with
  | Some f -> f ()
  | None ->
      if Sys.file_exists build_dir then
        failwith ("build-dir already exists: " ^ build_dir);
      default_dune_build ~caps ~root ~build_dir ~target
        ~verbose_log:(build_dir ^ ".verbose.log")
        ());
  let log =
    let verbose = build_dir ^ ".verbose.log" in
    if Sys.file_exists verbose then verbose else Filename.concat build_dir "log"
  in
  if not (Sys.file_exists binary_path) then
    failwith ("binary missing after build: " ^ binary_path);
  if not (Sys.file_exists log) then failwith ("dune log missing: " ^ log);
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps log in
  let log_text =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let linked_paths =
    parse_pie_link_cmxa ~expected_output:binary_path ~root ~build_dir log_text
    |> List.map (resolve_path ~cwd:root)
    |> ox_linked
  in
  if linked_paths = [] then
    failwith "no ox native archives on the pie.exe ocamlopt link line";
  List.iter
    (fun p ->
      if not (Sys.file_exists p) then failwith ("linked archive missing: " ^ p))
    linked_paths;
  let linked_native = visible_native_inputs ~caps linked_paths in
  List.iter
    (fun p ->
      if not (Sys.file_exists p) then failwith ("native input missing: " ^ p))
    linked_native;
  let source_after = source_fingerprint ~caps root in
  if not (String.equal source_before source_after) then
    failwith "source bytes changed during build; provenance record refused";
  let ox_env_after =
    hash_map ~caps (visible_native_inputs ~caps (find_ox_cmxa_env ~caps ()))
  in
  if ox_env_before <> ox_env_after then
    failwith "Ox archives changed during build; provenance record refused";
  (match (stub_before, find_tls_stub ~caps ()) with
  | Some (p, h), Some p2 when p = p2 && sha256_file ~caps p2 <> h ->
      failwith "TLS stub changed during build; provenance record refused"
  | Some (p, _), Some p2 when p <> p2 ->
      failwith "TLS stub path changed during build; provenance record refused"
  | Some _, None -> failwith "TLS stub disappeared during build"
  | _ -> ());
  List.iter
    (fun p ->
      match List.assoc_opt p ox_env_before with
      | Some h when h = sha256_file ~caps p -> ()
      | Some _ -> failwith ("linked Ox archive changed during build: " ^ p)
      | None -> failwith ("linked Ox archive had no pre-build hash: " ^ p))
    linked_native;
  let linked = List.map (id_of_path ~caps) linked_paths in
  let companions =
    List.map (id_of_path ~caps) (List.map companion_archive linked_paths)
  in
  let tls_native =
    Option.map (id_of_path ~caps) (linked_tls_stub_archive linked_paths)
  in
  let stublibs = Option.map (id_of_path ~caps) (find_tls_stub ~caps ()) in
  let json =
    `Assoc
      [
        ("compiler", `String (ocaml_version ~caps ()));
        ("dune", `String (dune_version ~caps ()));
        ( "build",
          `Assoc
            [
              ("build_dir", `String build_dir);
              ("target", `String target);
              ("dune_cache", `String "disabled");
            ] );
        ( "pie_source",
          `Assoc
            [
              ("head", `String (git_head ~caps root));
              ("dirty", `Bool (git_dirty ~caps root));
              ("fingerprint", `String source_after);
            ] );
        ("linked_native_archives", `List (List.map file_id_json linked));
        ("linked_companion_archives", `List (List.map file_id_json companions));
        ( "tls_native_stub_archive",
          match tls_native with None -> `Null | Some e -> file_id_json e );
        ( "runtime_stublibs",
          match stublibs with None -> `Null | Some e -> file_id_json e );
        ( "binary",
          `Assoc
            [
              ("path", `String binary_path);
              ("sha256", `String (sha256_file ~caps binary_path));
              ("build_identity", embedded_build_identity ~caps build_dir);
            ] );
      ]
    |> Yojson.Safe.pretty_to_string
  in
  atomic_write ~caps output (json ^ "\n");
  binary_path
