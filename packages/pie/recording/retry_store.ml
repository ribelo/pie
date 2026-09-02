type mode = Transcribe | Translate
type t = { mode : mode; path : string; mutable output : out_channel option }
type pending = { mode : mode; path : string; pcm_bytes : bytes }

let string_of_mode = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

let mode_of_filename name =
  if String.starts_with ~prefix:"transcribe-" name then Some Transcribe
  else if String.starts_with ~prefix:"translate-" name then Some Translate
  else None

let default_dir = Paths.failed_recordings_dir
let counter = Atomic.make 0

let create ?(dir = default_dir) mode =
  let sequence = Atomic.fetch_and_add counter 1 in
  let timestamp_us = int_of_float (Unix.gettimeofday () *. 1_000_000.0) in
  let name =
    Printf.sprintf "%s-%d-%d-%d.pcm" (string_of_mode mode) timestamp_us
      (Unix.getpid ()) sequence
  in
  { mode; path = Filename.concat dir name; output = None }

let ensure_output recording =
  match recording.output with
  | Some output -> output
  | None ->
      Fs.mkdir_p (Filename.dirname recording.path);
      let output = open_out_bin recording.path in
      recording.output <- Some output;
      output

let append recording bytes =
  let output = ensure_output recording in
  output_bytes output bytes;
  flush output

let close recording =
  match recording.output with
  | None -> ()
  | Some output ->
      recording.output <- None;
      close_out_noerr output

let discard path = try Sys.remove path with Sys_error _ -> ()

let read path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
      Bytes.of_string (really_input_string input (in_channel_length input)))

let latest ?(dir = default_dir) () =
  if not (Sys.file_exists dir && Sys.is_directory dir) then None
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter_map (fun name ->
        match mode_of_filename name with
        | None -> None
        | Some mode -> (
            let path = Filename.concat dir name in
            try
              let stat = Unix.stat path in
              if stat.st_kind = Unix.S_REG && stat.st_size > 0 then
                Some (stat.st_mtime, mode, path)
              else None
            with Unix.Unix_error _ -> None))
    |> List.sort (fun (left, _, _) (right, _, _) -> Float.compare right left)
    |> function
    | [] -> None
    | (_, mode, path) :: _ -> Some { mode; path; pcm_bytes = read path }
