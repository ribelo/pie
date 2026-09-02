type process_info = {
  pid : int;
  ppid : int;
  pgrp : int;
  tty_nr : int;
  tpgid : int;
  depth : int;
  cwd : string option;
}

let int_of_string_opt value =
  try Some (int_of_string value) with Failure _ -> None

let proc_path pid name =
  Filename.concat (Filename.concat "/proc" (string_of_int pid)) name

let read_link_opt path =
  try Some (Unix.readlink path) with Unix.Unix_error _ -> None

let read_file_opt path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  with Sys_error _ -> None

let parse_proc_stat_line line =
  match String.index_opt line '(' with
  | None -> None
  | Some open_paren -> (
      match String.rindex_opt line ')' with
      | None -> None
      | Some close_paren when close_paren <= open_paren -> None
      | Some close_paren -> (
          let pid_text = String.sub line 0 open_paren |> String.trim in
          let fields =
            String.sub line (close_paren + 1)
              (String.length line - close_paren - 1)
            |> String.trim |> String.split_on_char ' '
            |> List.filter (fun item -> item <> "")
          in
          match (int_of_string_opt pid_text, fields) with
          | Some pid, _state :: ppid :: pgrp :: _session :: tty_nr :: tpgid :: _
            ->
              Option.bind (int_of_string_opt ppid) (fun ppid ->
                  Option.bind (int_of_string_opt pgrp) (fun pgrp ->
                      Option.bind (int_of_string_opt tty_nr) (fun tty_nr ->
                          Option.map
                            (fun tpgid -> (pid, ppid, pgrp, tty_nr, tpgid))
                            (int_of_string_opt tpgid))))
          | _ -> None))

let cwd_for_process pid = read_link_opt (proc_path pid "cwd")

let read_process_stat pid =
  match read_file_opt (proc_path pid "stat") with
  | None -> None
  | Some stat ->
      Option.map
        (fun (pid, ppid, pgrp, tty_nr, tpgid) ->
          {
            pid;
            ppid;
            pgrp;
            tty_nr;
            tpgid;
            depth = 0;
            cwd = cwd_for_process pid;
          })
        (parse_proc_stat_line stat)

let list_process_stats () =
  try
    Sys.readdir "/proc" |> Array.to_list
    |> List.filter_map int_of_string_opt
    |> List.filter_map read_process_stat
  with Sys_error _ -> []

let descendants_of root_pid processes =
  let rec loop queue acc =
    match queue with
    | [] -> acc
    | (pid, depth) :: rest ->
        let children =
          processes
          |> List.filter (fun process -> process.ppid = pid)
          |> List.map (fun process -> { process with depth })
        in
        loop
          (rest @ List.map (fun process -> (process.pid, depth + 1)) children)
          (children @ acc)
  in
  loop [ (root_pid, 1) ] []

let deepest_first processes =
  List.sort
    (fun left right ->
      match compare right.depth left.depth with
      | 0 -> compare right.pid left.pid
      | order -> order)
    processes

let cwd_from_processes ~root_pid ~root_cwd processes =
  let root =
    match List.find_opt (fun process -> process.pid = root_pid) processes with
    | Some root -> { root with depth = 0 }
    | None ->
        {
          pid = root_pid;
          ppid = 0;
          pgrp = 0;
          tty_nr = 0;
          tpgid = 0;
          depth = 0;
          cwd = root_cwd;
        }
  in
  let tty_descendants =
    descendants_of root_pid processes
    |> List.filter (fun process -> process.tty_nr <> 0)
  in
  let foreground =
    tty_descendants
    |> List.filter (fun process ->
        process.tpgid > 0 && process.pgrp = process.tpgid)
    |> deepest_first
  in
  let candidates =
    if tty_descendants = [] then [ root ]
    else foreground @ deepest_first tty_descendants @ [ root ]
  in
  candidates |> List.find_map (fun process -> process.cwd)

let cwd_for_pid root_pid =
  cwd_from_processes ~root_pid ~root_cwd:(cwd_for_process root_pid)
    (list_process_stats ())

let context_path dir = Filename.concat (Filename.concat dir ".pie") "context.md"

let rec find_context_file dir =
  let candidate = context_path dir in
  if Sys.file_exists candidate && not (Sys.is_directory candidate) then
    Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_context_file parent

let find_context_file dir =
  (* Resolve relative paths (e.g. ".") against the current working directory so
     that parent traversal can actually climb above the starting directory.
     [Filename.dirname "."] is ".", which would otherwise stop traversal at the
     starting directory. *)
  let absolute =
    if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
    else dir
  in
  find_context_file absolute

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let load_for_cwd cwd =
  match find_context_file cwd with
  | None -> None
  | Some path -> (
      try
        let content = read_file path |> String.trim in
        if content = "" then None else Some content
      with Sys_error _ -> None)

let load_for_pid pid = Option.bind (cwd_for_pid pid) load_for_cwd

let append_to_prompt prompt_template context =
  match context with
  | None -> prompt_template
  | Some context ->
      prompt_template ^ "\n\nProject context from .pie/context.md:\n" ^ context
