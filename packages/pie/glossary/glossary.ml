type unreadable = { path : string; message : string }
type warning = Unreadable of unreadable
type t = { text : string; paths : string list; warnings : warning list }

type status = {
  path : string;
  exists : bool option;
  line_count : int option;
  warnings : warning list;
}

let absolute path =
  if Filename.is_relative path then
    match Sys.getcwd () with
    | cwd -> Filename.concat cwd path
    | exception Sys_error _ -> path
  else path

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

let kind_message = function
  | Unix.S_REG -> "not a regular file"
  | Unix.S_DIR -> "is a directory"
  | Unix.S_CHR -> "is a character device"
  | Unix.S_BLK -> "is a block device"
  | Unix.S_LNK -> "is a symlink"
  | Unix.S_FIFO -> "is a fifo"
  | Unix.S_SOCK -> "is a socket"

type glossary_file = Missing | Read of string | Failed of bool option * string

let read_glossary ~caps path =
  match Unix.lstat path with
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Missing
  | exception Unix.Unix_error (err, _, _) ->
      Failed (None, Unix.error_message err)
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
      | Error message -> Failed (Some true, message)
      | Ok st when st.st_kind <> Unix.S_REG ->
          Failed (Some true, kind_message st.st_kind)
      | Ok _ -> (
          try Read (read_all ~caps path) with
          | Unix.Unix_error (err, _, _) ->
              Failed (Some true, Unix.error_message err)
          | Sys_error message -> Failed (Some true, message)))

let global_path ~caps =
  let default () = Filename.concat (Paths.config_dir ~caps) "glossary.md" in
  absolute
    (match Ox_caps.Cap_sys.getenv_opt ~caps "PIE_GLOSSARY" with
    | Some raw when String.trim raw <> "" -> String.trim raw
    | _ -> default ())

let warning path message = Unreadable { path; message }
let missing_warning path = warning path "file is missing"

let resolve ~caps () : t =
  let path = global_path ~caps in
  match read_glossary ~caps path with
  | Missing -> { text = ""; paths = []; warnings = [ missing_warning path ] }
  | Failed (_, message) ->
      { text = ""; paths = []; warnings = [ warning path message ] }
  | Read content ->
      let content =
        if content = "" || String.ends_with ~suffix:"\n" content then content
        else content ^ "\n"
      in
      { text = "# " ^ path ^ "\n\n" ^ content; paths = [ path ]; warnings = [] }

let line_count content =
  let newlines =
    String.fold_left
      (fun count ch -> if ch = '\n' then count + 1 else count)
      0 content
  in
  if content = "" || String.ends_with ~suffix:"\n" content then newlines
  else newlines + 1

let status ~caps () =
  let path = global_path ~caps in
  match read_glossary ~caps path with
  | Missing ->
      {
        path;
        exists = Some false;
        line_count = Some 0;
        warnings = [ missing_warning path ];
      }
  | Failed (exists, message) ->
      { path; exists; line_count = None; warnings = [ warning path message ] }
  | Read content ->
      {
        path;
        exists = Some true;
        line_count = Some (line_count content);
        warnings = [];
      }
