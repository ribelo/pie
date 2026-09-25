(* Build-interval provenance for pie.exe. *)

let contains needle hay =
  let n = String.length needle in
  let h = String.length hay in
  let rec loop i =
    i <= h - n && (String.sub hay i n = needle || loop (i + 1))
  in
  loop 0

let read_process cmd =
  let ic = Unix.open_process_in cmd in
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

let sha256_file path =
  let out = read_process (Filename.quote_command "sha256sum" [ path ]) in
  match String.split_on_char ' ' out with
  | hash :: _ -> hash
  | [] -> failwith ("sha256sum failed: " ^ path)

let ocaml_version () = read_process "ocaml -version"
let dune_version () = read_process "dune --version"

let git_head root =
  if Sys.file_exists (Filename.concat root ".git") then
    read_process
      (Filename.quote_command "git" [ "-C"; root; "rev-parse"; "HEAD" ])
  else "unknown"

let git_dirty root =
  if not (Sys.file_exists (Filename.concat root ".git")) then true
  else
    let st =
      read_process
        (Filename.quote_command "git" [ "-C"; root; "status"; "--porcelain" ])
    in
    st <> ""

let rec collect_files acc dir =
  match Unix.opendir dir with
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
                    else loop (collect_files acc path)
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

let fingerprint_files root paths =
  let rels =
    paths
    |> List.map (fun p -> (relative_to root p, p))
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let lines = List.map (fun (rel, abs) -> sha256_file abs ^ "  " ^ rel) rels in
  let tmp = Filename.temp_file "pie-fp" "" in
  Fun.protect
    ~finally:(fun () -> try Unix.unlink tmp with _ -> ())
    (fun () ->
      let oc = open_out_bin tmp in
      List.iter
        (fun line ->
          output_string oc line;
          output_char oc '\n')
        lines;
      close_out oc;
      sha256_file tmp)

let config_inputs root =
  [
    "dune-project";
    "dune-workspace";
    "packages/pie/dune";
    "packages/pie/bin/dune";
  ]
  |> List.filter_map (fun n ->
      let p = Filename.concat root n in
      if Sys.file_exists p then Some p else None)

let source_paths root =
  collect_files [] (Filename.concat root "packages/pie") @ config_inputs root

let source_fingerprint root = fingerprint_files root (source_paths root)

let ocamlpath_dirs () =
  match Sys.getenv_opt "OCAMLPATH" with
  | None | Some "" -> []
  | Some p -> String.split_on_char ':' p |> List.filter (( <> ) "")

let caml_ld_dirs () =
  match Sys.getenv_opt "CAML_LD_LIBRARY_PATH" with
  | None | Some "" -> []
  | Some p -> String.split_on_char ':' p |> List.filter (( <> ) "")

let find_eta_cmxa_env () =
  ocamlpath_dirs ()
  |> List.concat_map (fun dir ->
      match Sys.readdir dir with
      | exception _ -> []
      | names ->
          Array.to_list names
          |> List.filter_map (fun name ->
              let lower = String.lowercase_ascii name in
              if String.length lower < 3 || String.sub lower 0 3 <> "eta" then
                None
              else
                let p =
                  Filename.concat (Filename.concat dir name) (name ^ ".cmxa")
                in
                if Sys.file_exists p then Some p else None))

let find_tls_stub () =
  caml_ld_dirs ()
  |> List.find_map (fun dir ->
      let stub = Filename.concat dir "dlleta_http_tls_openssl_stubs.so" in
      if Sys.file_exists stub then Some stub else None)

let hash_map paths =
  paths
  |> List.map (fun p -> (p, sha256_file p))
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

let eta_linked paths =
  List.filter
    (fun p ->
      let base = String.lowercase_ascii (Filename.basename p) in
      String.length base >= 3 && String.sub base 0 3 = "eta")
    paths

let companion_archive cmxa =
  if not (Filename.check_suffix cmxa ".cmxa") then
    failwith ("not a cmxa: " ^ cmxa);
  let archive = Filename.chop_suffix cmxa ".cmxa" ^ ".a" in
  if not (Sys.file_exists archive) then
    failwith ("missing companion archive: " ^ archive);
  archive

let stub_archives_in dir =
  match Sys.readdir dir with
  | exception _ -> []
  | names ->
      Array.to_list names
      |> List.filter_map (fun name ->
          let lower = String.lowercase_ascii name in
          if Filename.check_suffix lower ".a" && contains "stubs" lower then
            Some (Filename.concat dir name)
          else None)

let package_stub_archives dir =
  match stub_archives_in dir with
  | [] -> []
  | [ path ] -> [ path ]
  | many ->
      failwith
        ("ambiguous native C stub archives in " ^ dir ^ ": "
       ^ String.concat ", " many)

let package_native_inputs cmxa =
  companion_archive cmxa :: package_stub_archives (Filename.dirname cmxa)

let visible_native_inputs cmxa_paths =
  List.concat_map (fun cmxa -> cmxa :: package_native_inputs cmxa) cmxa_paths

let known_tls_cmxa_name = "eta_http_tls_openssl.cmxa"
let known_tls_stub_name = "libeta_http_tls_openssl_stubs.a"

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
        ("duplicate linked eta_http_tls_openssl.cmxa versions: "
       ^ String.concat ", " many)

type file_id = { name : string; path : string; sha256 : string }

let id_of_path path =
  { name = Filename.basename path; path; sha256 = sha256_file path }

let file_id_json e : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String e.name);
      ("path", `String e.path);
      ("sha256", `String e.sha256);
    ]

let unique_candidate dir =
  let rec loop n =
    if n > 1024 then failwith "could not allocate provenance candidate"
    else
      let path =
        Filename.concat dir
          (Printf.sprintf ".pie-prov-%d-%d.json" (Unix.getpid ()) n)
      in
      match
        Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600
      with
      | fd -> (path, fd)
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n + 1)
  in
  loop 0

let atomic_write path contents =
  let dir = Filename.dirname path in
  let cand, fd = unique_candidate dir in
  let oc = Unix.out_channel_of_descr fd in
  let published = ref false in
  Fun.protect
    ~finally:(fun () ->
      (try close_out_noerr oc with _ -> ());
      if not !published then try Unix.unlink cand with _ -> ())
    (fun () ->
      output_string oc contents;
      flush oc;
      close_out oc;
      Unix.rename cand path;
      published := true)

let default_dune_build ~root ~build_dir ~target ~verbose_log () =
  let old = Unix.getcwd () in
  Unix.chdir root;
  Fun.protect
    ~finally:(fun () -> Unix.chdir old)
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
      match Unix.system redirected with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n -> failwith ("dune build exited " ^ string_of_int n)
      | _ -> failwith "dune build failed")

let write_record ~root ~build_dir ~target ~output ?run_build () =
  let source_before = source_fingerprint root in
  let eta_cmxa_env = find_eta_cmxa_env () in
  let eta_env_before = hash_map (visible_native_inputs eta_cmxa_env) in
  let stub_before =
    match find_tls_stub () with
    | None -> None
    | Some p -> Some (p, sha256_file p)
  in
  let binary_path =
    Filename.concat build_dir (Filename.concat "default" target)
  in
  (match run_build with
  | Some f -> f ()
  | None ->
      if Sys.file_exists build_dir then
        failwith ("build-dir already exists: " ^ build_dir);
      default_dune_build ~root ~build_dir ~target
        ~verbose_log:(build_dir ^ ".verbose.log")
        ());
  let log =
    let verbose = build_dir ^ ".verbose.log" in
    if Sys.file_exists verbose then verbose else Filename.concat build_dir "log"
  in
  if not (Sys.file_exists binary_path) then
    failwith ("binary missing after build: " ^ binary_path);
  if not (Sys.file_exists log) then failwith ("dune log missing: " ^ log);
  let ic = open_in_bin log in
  let log_text =
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let linked_paths =
    parse_pie_link_cmxa ~expected_output:binary_path ~root ~build_dir log_text
    |> List.map (resolve_path ~cwd:root)
    |> eta_linked
  in
  if linked_paths = [] then
    failwith "no eta native archives on the pie.exe ocamlopt link line";
  List.iter
    (fun p ->
      if not (Sys.file_exists p) then failwith ("linked archive missing: " ^ p))
    linked_paths;
  let linked_native = visible_native_inputs linked_paths in
  List.iter
    (fun p ->
      if not (Sys.file_exists p) then failwith ("native input missing: " ^ p))
    linked_native;
  let source_after = source_fingerprint root in
  if not (String.equal source_before source_after) then
    failwith "source bytes changed during build; provenance record refused";
  let eta_env_after = hash_map (visible_native_inputs (find_eta_cmxa_env ())) in
  if eta_env_before <> eta_env_after then
    failwith "Eta archives changed during build; provenance record refused";
  (match (stub_before, find_tls_stub ()) with
  | Some (p, h), Some p2 when p = p2 && sha256_file p2 <> h ->
      failwith "TLS stub changed during build; provenance record refused"
  | Some (p, _), Some p2 when p <> p2 ->
      failwith "TLS stub path changed during build; provenance record refused"
  | Some _, None -> failwith "TLS stub disappeared during build"
  | _ -> ());
  List.iter
    (fun p ->
      match List.assoc_opt p eta_env_before with
      | Some h when h = sha256_file p -> ()
      | Some _ -> failwith ("linked Eta archive changed during build: " ^ p)
      | None -> failwith ("linked Eta archive had no pre-build hash: " ^ p))
    linked_native;
  let linked = List.map id_of_path linked_paths in
  let companions =
    List.map id_of_path (List.map companion_archive linked_paths)
  in
  let tls_native =
    Option.map id_of_path (linked_tls_stub_archive linked_paths)
  in
  let stublibs = Option.map id_of_path (find_tls_stub ()) in
  let json =
    `Assoc
      [
        ("compiler", `String (ocaml_version ()));
        ("dune", `String (dune_version ()));
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
              ("head", `String (git_head root));
              ("dirty", `Bool (git_dirty root));
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
              ("sha256", `String (sha256_file binary_path));
            ] );
      ]
    |> Yojson.Safe.pretty_to_string
  in
  atomic_write output (json ^ "\n");
  binary_path
