type unreadable = { path : string; message : string }
type warning = Unreadable of unreadable | Focus_unavailable of string

type t = {
  text : string;
  paths : string list;
  warnings : warning list;
  cwd : string option;
}

let strip_slashes path =
  if path = "" || path = "/" then path
  else
    let rec loop s =
      let n = String.length s in
      if n > 1 && s.[n - 1] = '/' then loop (String.sub s 0 (n - 1)) else s
    in
    loop path

let absolute path =
  if Filename.is_relative path then
    match Sys.getcwd () with
    | cwd -> Filename.concat cwd path
    | exception Sys_error _ -> path
  else path

let normalize path =
  let path = strip_slashes path in
  if path = "" then path
  else
    match Unix.realpath path with
    | real -> real
    | exception Unix.Unix_error _ -> absolute path

let proc_path pid name =
  Filename.concat (Filename.concat "/proc" (string_of_int pid)) name

(* /proc reports a size of 0, so [in_channel_length] reads nothing. Stream
   until EOF. *)
let read_all ~caps path =
  let fd =
    Ox_caps.Cap_unix.openfile ~caps path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0
  in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      let buf = Bytes.create 65536 in
      let out = Buffer.create 256 in
      let rec loop () =
        match Unix.read fd buf 0 (Bytes.length buf) with
        | 0 -> Buffer.contents out
        | n ->
            Buffer.add_subbytes out buf 0 n;
            loop ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
      in
      loop ())

let read_proc ~caps path =
  try Some (read_all ~caps path) with Unix.Unix_error _ -> None

let field index fields =
  let rec loop i = function
    | [] -> None
    | value :: _ when i = index -> Some value
    | _ :: rest -> loop (i + 1) rest
  in
  loop 0 fields

let parse_stat text =
  match (String.index_opt text '(', String.rindex_opt text ')') with
  | Some open_paren, Some close_paren when close_paren > open_paren -> (
      let pid_text = String.trim (String.sub text 0 open_paren) in
      let rest =
        String.sub text (close_paren + 1) (String.length text - close_paren - 1)
      in
      let fields =
        rest |> String.split_on_char ' ' |> List.filter (fun item -> item <> "")
      in
      (* After ')': index 1 is ppid (stat field 4); index 19 is starttime
         (stat field 22), clock ticks since boot. *)
      match
        ( int_of_string_opt pid_text,
          Option.bind (field 1 fields) int_of_string_opt,
          Option.bind (field 19 fields) Int64.of_string_opt )
      with
      | Some pid, Some ppid, Some starttime -> Some (pid, ppid, starttime)
      | _ -> None)
  | _ -> None

type proc = { pid : int; ppid : int; starttime : int64 }

let list_procs ~caps =
  match Ox_caps.Cap_sys.readdir ~caps "/proc" with
  | exception Sys_error _ -> []
  | names ->
      Array.fold_left
        (fun acc name ->
          match int_of_string_opt name with
          | None -> acc
          | Some pid -> (
              match read_proc ~caps (proc_path pid "stat") with
              | None -> acc
              | Some text -> (
                  match parse_stat text with
                  | Some (stat_pid, ppid, starttime) when stat_pid = pid ->
                      { pid; ppid; starttime } :: acc
                  | _ -> acc)))
        [] names

let children_of procs =
  let table = Hashtbl.create (max 16 (List.length procs)) in
  List.iter
    (fun proc ->
      let kids =
        match Hashtbl.find_opt table proc.ppid with
        | None -> []
        | Some kids -> kids
      in
      Hashtbl.replace table proc.ppid (proc :: kids))
    procs;
  table

let descendants root table =
  let visited = Hashtbl.create 16 in
  let rec collect pid acc =
    match Hashtbl.find_opt table pid with
    | None -> acc
    | Some kids ->
        List.fold_left
          (fun acc kid ->
            if Hashtbl.mem visited kid.pid then acc
            else (
              Hashtbl.add visited kid.pid ();
              collect kid.pid (kid :: acc)))
          acc kids
  in
  Hashtbl.add visited root ();
  collect root []

let leaves ~root procs descendants =
  let all =
    match List.find_opt (fun proc -> proc.pid = root) procs with
    | None -> descendants
    | Some proc -> proc :: descendants
  in
  List.filter
    (fun proc -> not (List.exists (fun other -> other.ppid = proc.pid) all))
    all

let by_recent left right =
  match Int64.compare right.starttime left.starttime with
  | 0 -> compare right.pid left.pid
  | order -> order

let cwd_link pid =
  try Some (Unix.readlink (proc_path pid "cwd"))
  with Unix.Unix_error _ -> None

let cwd_for_pid ~caps pid =
  let procs = list_procs ~caps in
  let ordered =
    leaves ~root:pid procs (descendants pid (children_of procs))
    |> List.sort by_recent
  in
  (* Skip a leaf whose cwd cannot be read and try the next most recent.
     Equal start times prefer the higher pid so the choice is stable. *)
  match List.find_map (fun proc -> cwd_link proc.pid) ordered with
  | Some cwd -> Some (normalize cwd)
  | None -> Option.map normalize (cwd_link pid)

let kind_message = function
  | Unix.S_REG -> "not a regular file"
  | Unix.S_DIR -> "is a directory"
  | Unix.S_CHR -> "is a character device"
  | Unix.S_BLK -> "is a block device"
  | Unix.S_LNK -> "is a symlink"
  | Unix.S_FIFO -> "is a fifo"
  | Unix.S_SOCK -> "is a socket"

type glossary_file = Missing | Read of string | Failed of string

let read_glossary ~caps path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Missing
  | exception Unix.Unix_error (Unix.ENOTDIR, _, _) -> Missing
  | exception Unix.Unix_error (err, _, _) -> Failed (Unix.error_message err)
  | link -> (
      let followed =
        if link.st_kind = Unix.S_LNK then
          match Unix.stat path with
          | st -> Ok st
          | exception Unix.Unix_error (err, _, _) ->
              Error (Unix.error_message err)
        else Ok link
      in
      match followed with
      | Error message -> Failed message
      | Ok st when st.st_kind <> Unix.S_REG -> Failed (kind_message st.st_kind)
      | Ok _ -> (
          try Read (read_all ~caps path) with
          | Unix.Unix_error (err, _, _) -> Failed (Unix.error_message err)
          | Sys_error message -> Failed message))

let default_global ~caps =
  absolute
    (Filename.concat
       (Filename.dirname (Paths.stt_config_path ~caps))
       "glossary.md")

let global_path ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "PIE_GLOSSARY" with
  | Some raw ->
      let path = String.trim raw in
      if path = "" then default_global ~caps else absolute path
  | None -> default_global ~caps

let home_dir ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "HOME" with
  | Some raw ->
      let path = String.trim raw in
      if path = "" then None else Some (normalize path)
  | None -> None

let is_boundary ~home dir =
  dir = "/" || match home with Some home -> dir = home | None -> false

let project_candidate dir =
  Filename.concat (Filename.concat dir ".pie") "glossary.md"

let find_project ~caps ~home start warnings =
  let rec walk dir warnings =
    let candidate = project_candidate dir in
    match read_glossary ~caps candidate with
    | Missing ->
        if is_boundary ~home dir then (None, warnings)
        else
          let parent = Filename.dirname dir in
          if parent = dir then (None, warnings) else walk parent warnings
    | Failed message ->
        (None, Unreadable { path = candidate; message } :: warnings)
    | Read content -> (Some (candidate, content), warnings)
  in
  walk (normalize start) warnings

let take_file ~caps path warnings =
  match read_glossary ~caps path with
  | Missing -> (None, warnings)
  | Failed message -> (None, Unreadable { path; message } :: warnings)
  | Read content -> (Some (path, content), warnings)

let ensure_nl text =
  if text = "" || String.ends_with ~suffix:"\n" text then text else text ^ "\n"

let section path content = "# " ^ path ^ "\n\n" ^ ensure_nl content

let render parts =
  match parts with
  | [] -> ""
  | parts ->
      String.concat "\n"
        (List.map (fun (path, content) -> section path content) parts)

let resolve ~caps ~pid =
  let cwd = Option.bind pid (cwd_for_pid ~caps) in
  let global, warnings = take_file ~caps (global_path ~caps) [] in
  let project, warnings =
    match cwd with
    | None -> (None, warnings)
    | Some dir -> find_project ~caps ~home:(home_dir ~caps) dir warnings
  in
  let parts = List.filter_map Fun.id [ global; project ] in
  {
    text = render parts;
    paths = List.map fst parts;
    warnings = List.rev warnings;
    cwd;
  }

let resolve_focused ~caps ~process_mgr ?(on_focus = fun _ -> ()) () =
  match Niri.focused_window ~process_mgr () with
  | Error (`Niri_error message) ->
      let resolution = resolve ~caps ~pid:None in
      {
        resolution with
        warnings = Focus_unavailable message :: resolution.warnings;
      }
  | Ok None ->
      on_focus None;
      resolve ~caps ~pid:None
  | Ok (Some window) ->
      on_focus (Some window);
      resolve ~caps ~pid:window.pid
