let sample_rate = 16_000
let encode_timeout_s = 120.
let dir ~caps = Filename.concat (Paths.data_dir ~caps) "audio"

let is_safe_id id =
  let n = String.length id in
  if n = 0 || n > 128 || String.equal id "." || String.equal id ".." then false
  else
    let rec loop i =
      if i = n then true
      else
        match id.[i] with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> loop (i + 1)
        | _ -> false
    in
    loop 0

let extension = function `Opus -> ".opus" | `Wav -> ".wav"
let destination ~dir ~id ~ext = Filename.concat dir (id ^ extension ext)

let executable path =
  match Unix.stat path with
  | { Unix.st_kind = Unix.S_REG; _ } -> (
      match Unix.access path [ Unix.X_OK ] with
      | () -> true
      | exception Unix.Unix_error _ -> false)
  | _ -> false
  | exception Unix.Unix_error _ -> false

let resolve_ffmpeg ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "PIE_FFMPEG" with
  | Some raw when String.trim raw <> "" ->
      let path = String.trim raw in
      if executable path then Some path else None
  | _ -> Sh.find_executable ~caps "ffmpeg"

let predict_path ~caps ~dir ~id =
  let ext = match resolve_ffmpeg ~caps with Some _ -> `Opus | None -> `Wav in
  destination ~dir ~id ~ext

let ( let* ) = Result.bind
let err message = Error message

(* Create only missing ancestors at 0700. Existing directories are left
   untouched, including a historical 0755 parent. A symbolic link is not a
   directory and is not followed. *)
let rec ensure_private_missing ~caps path =
  if path = "" || path = Filename.dirname path then ()
  else
    match Unix.lstat path with
    | { st_kind = Unix.S_DIR; _ } -> ()
    | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        ensure_private_missing ~caps (Filename.dirname path);
        try Ox_caps.Cap_unix.mkdir ~caps path 0o700
        with Unix.Unix_error (Unix.EEXIST, _, _) -> (
          match Unix.lstat path with
          | { st_kind = Unix.S_DIR; _ } -> ()
          | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))))

let ensure_dir ~caps dir =
  try
    ensure_private_missing ~caps dir;
    Ok ()
  with
  | Unix.Unix_error (error, _, _) ->
      err
        ("could not create audio archive directory: " ^ Unix.error_message error)
  | Sys_error message ->
      err ("could not create audio archive directory: " ^ message)

let temp_counter = Atomic.make 0

let temp_name path =
  let n = Atomic.fetch_and_add temp_counter 1 in
  Printf.sprintf "%s.%d.%d.partial" path (Unix.getpid ()) n

let unlink_quiet ~caps path =
  try Ox_caps.Cap_unix.unlink ~caps path with Unix.Unix_error _ -> ()

let rec write_all fd buf off len =
  if len > 0 then
    let wrote = Unix.single_write fd buf off len in
    if wrote <= 0 then raise (Sys_error "short write to audio archive")
    else write_all fd buf (off + wrote) (len - wrote)

let write_private ~caps path payload =
  let tmp = temp_name path in
  try
    let fd =
      Ox_caps.Cap_unix.openfile_write ~caps tmp
        [ Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
        0o600
    in
    let close_fd () = try Unix.close fd with Unix.Unix_error _ -> () in
    match
      try
        Unix.fchmod fd 0o600;
        write_all fd payload 0 (Bytes.length payload);
        Unix.fsync fd;
        close_fd ();
        Ok ()
      with exn ->
        close_fd ();
        Error exn
    with
    | Error exn ->
        unlink_quiet ~caps tmp;
        raise exn
    | Ok () -> (
        match Unix.lstat tmp with
        | { Unix.st_kind = Unix.S_REG; _ } ->
            Ox_caps.Cap_unix.rename ~caps tmp path;
            Ok path
        | _ ->
            unlink_quiet ~caps tmp;
            err "audio archive temp file is not a regular file")
  with
  | Unix.Unix_error (error, _, _) ->
      unlink_quiet ~caps tmp;
      err ("audio archive: " ^ Unix.error_message error)
  | Sys_error message ->
      unlink_quiet ~caps tmp;
      err ("audio archive: " ^ message)

let wav_bytes pcm =
  let header =
    Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm) ~sample_rate
  in
  let out = Bytes.create (Bytes.length header + Bytes.length pcm) in
  Bytes.blit header 0 out 0 (Bytes.length header);
  Bytes.blit pcm 0 out (Bytes.length header) (Bytes.length pcm);
  out

let opus_args output =
  [
    "-hide_banner";
    "-loglevel";
    "error";
    "-y";
    "-f";
    "s16le";
    "-ar";
    "16000";
    "-ac";
    "1";
    "-i";
    "pipe:0";
    "-c:a";
    "libopus";
    "-b:a";
    "24k";
    "-application";
    "voip";
    (* The temporary name ends in .partial, so ffmpeg cannot infer the
       container from the extension. *)
    "-f";
    "ogg";
    output;
  ]

let failure_text ~ffmpeg exn =
  try Sh.failure_message ~prog:ffmpeg exn with _ -> Printexc.to_string exn

let encode_opus ~process_mgr ~ffmpeg ~caps path pcm =
  let tmp = temp_name path in
  try
    Eio.Process.run process_mgr ~executable:ffmpeg
      ~stdin:(Eio.Flow.string_source (Bytes.to_string pcm))
      ~stdout:(Sh.discarded_stderr ()) ~stderr:(Sh.discarded_stderr ())
      (ffmpeg :: opus_args tmp);
    match Unix.lstat tmp with
    | { Unix.st_kind = Unix.S_REG; _ } ->
        Ox_caps.Cap_unix.chmod ~caps tmp 0o600;
        Ox_caps.Cap_unix.rename ~caps tmp path;
        Ok ()
    | _ ->
        unlink_quiet ~caps tmp;
        err "ffmpeg did not write a regular audio file"
  with
  | Eio.Cancel.Cancelled _ as exn ->
      unlink_quiet ~caps tmp;
      raise exn
  | exn ->
      unlink_quiet ~caps tmp;
      err (failure_text ~ffmpeg exn)

(* MP3 for provider upload: 32 kbit/s mono keeps a twelve-minute dictation
   under 3 MB, where the WAV is 23 MB. OpenRouter's Responses API accepts wav
   and mp3 but rejects ogg/opus. *)
let encode_mp3 ~caps ~process_mgr pcm =
  match resolve_ffmpeg ~caps with
  | None -> err "no ffmpeg"
  | Some ffmpeg -> (
      try
        let mp3 =
          Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
            ~executable:ffmpeg
            ~stdin:(Eio.Flow.string_source (Bytes.to_string pcm))
            ~stderr:(Sh.discarded_stderr ())
            (ffmpeg
            :: [
                 "-hide_banner";
                 "-loglevel";
                 "error";
                 "-f";
                 "s16le";
                 "-ar";
                 "16000";
                 "-ac";
                 "1";
                 "-i";
                 "pipe:0";
                 "-c:a";
                 "libmp3lame";
                 "-b:a";
                 "32k";
                 "-f";
                 "mp3";
                 "pipe:1";
               ])
        in
        if mp3 = "" then err "ffmpeg wrote no mp3" else Ok (Bytes.of_string mp3)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> err (failure_text ~ffmpeg exn))

let store ~caps ~process_mgr ~dir ~id pcm =
  if not (is_safe_id id) then err ("unknown archive id " ^ id)
  else
    let* () = ensure_dir ~caps dir in
    let write_wav () =
      write_private ~caps (destination ~dir ~id ~ext:`Wav) (wav_bytes pcm)
    in
    match resolve_ffmpeg ~caps with
    | None -> write_wav ()
    | Some ffmpeg -> (
        let opus = destination ~dir ~id ~ext:`Opus in
        match encode_opus ~process_mgr ~ffmpeg ~caps opus pcm with
        | Ok () -> Ok opus
        | Error _ -> write_wav ())

let read_file ~caps path =
  Ox_caps.Cap_stdlib.In_channel.with_open_bin ~caps path In_channel.input_all

let parse_wav payload =
  let bytes = Bytes.of_string payload in
  let n = Bytes.length bytes in
  if n < 44 then err "archived wav is too short"
  else
    let data_length = n - 44 in
    let expected = Audio.Pcm.build_pcm_wav_header ~data_length ~sample_rate in
    if not (Bytes.equal (Bytes.sub bytes 0 44) expected) then
      err "archived wav is not 16 kHz mono s16le"
    else Ok (Bytes.sub bytes 44 data_length)

let decode_opus ~caps ~process_mgr path =
  match resolve_ffmpeg ~caps with
  | None -> err "no decoder for archived audio"
  | Some ffmpeg -> (
      try
        let pcm =
          Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
            ~executable:ffmpeg ~stderr:(Sh.discarded_stderr ())
            (ffmpeg
            :: [
                 "-hide_banner";
                 "-loglevel";
                 "error";
                 "-i";
                 path;
                 "-f";
                 "s16le";
                 "-ar";
                 "16000";
                 "-ac";
                 "1";
                 "pipe:1";
               ])
        in
        Ok (Bytes.of_string pcm)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> err (failure_text ~ffmpeg exn))

let load ~caps ~process_mgr ~path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> err "no archived audio"
  | exception Unix.Unix_error (error, _, _) ->
      err ("audio archive: " ^ Unix.error_message error)
  | { Unix.st_kind = Unix.S_LNK; _ } -> err "archived audio is a symbolic link"
  | { Unix.st_kind = Unix.S_REG; _ } ->
      if Filename.check_suffix path ".wav" then parse_wav (read_file ~caps path)
      else if Filename.check_suffix path ".opus" then
        decode_opus ~caps ~process_mgr path
      else err "archived audio has an unknown extension"
  | _ -> err "archived audio is not a regular file"

type file_kind = Missing | File | Link | Other | Failed of string

let kind path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Missing
  | exception Unix.Unix_error (error, _, _) -> Failed (Unix.error_message error)
  | { st_kind = Unix.S_REG; _ } -> File
  | { st_kind = Unix.S_LNK; _ } -> Link
  | _ -> Other

type located = { path : string; ext : [ `Opus | `Wav ] }

let locate ~caps ~dir ~id =
  if not (is_safe_id id) then err ("unknown archive id " ^ id)
  else
    let opus = destination ~dir ~id ~ext:`Opus in
    let wav = destination ~dir ~id ~ext:`Wav in
    match (kind opus, kind wav) with
    | Failed message, _ | _, Failed message -> err ("audio archive: " ^ message)
    | File, _ -> (
        match resolve_ffmpeg ~caps with
        | Some _ -> Ok { path = opus; ext = `Opus }
        | None -> (
            match kind wav with
            | File -> Ok { path = wav; ext = `Wav }
            | _ -> err ("no decoder for archived audio " ^ id)))
    | Link, File -> Ok { path = wav; ext = `Wav }
    | Link, _ -> err ("archived audio for " ^ id ^ " is a symbolic link")
    | _, File -> Ok { path = wav; ext = `Wav }
    | _, Link -> err ("archived audio for " ^ id ^ " is a symbolic link")
    | _ -> err ("no archived audio for " ^ id)

let archive_name name =
  let id_of suffix =
    if Filename.check_suffix name suffix then
      Some (Filename.chop_suffix name suffix)
    else None
  in
  match id_of ".opus" with
  | Some id -> is_safe_id id
  | None -> (
      match id_of ".wav" with Some id -> is_safe_id id | None -> false)

let prune ~caps ~dir ~retention_hours ~now =
  if (not (Float.is_finite retention_hours)) || retention_hours <= 0. then Ok 0
  else if not (Float.is_finite now) then err "audio archive clock is not finite"
  else
    match Unix.lstat dir with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok 0
    | exception Unix.Unix_error (error, _, _) ->
        err ("audio archive: " ^ Unix.error_message error)
    | { st_kind = Unix.S_LNK; _ } ->
        err "audio archive directory is a symbolic link"
    | { st_kind = Unix.S_DIR; _ } -> (
        try
          let names = Ox_caps.Cap_sys.readdir ~caps dir in
          let limit = retention_hours *. 3600. in
          let removed = ref 0 in
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." && archive_name name then
                let path = Filename.concat dir name in
                match Unix.lstat path with
                | { st_kind = Unix.S_REG; st_mtime; _ }
                  when now -. st_mtime > limit ->
                    Ox_caps.Cap_unix.unlink ~caps path;
                    incr removed
                | _ -> ()
                | exception Unix.Unix_error _ -> ())
            names;
          Ok !removed
        with
        | Unix.Unix_error (error, _, _) ->
            err ("audio archive: " ^ Unix.error_message error)
        | Sys_error message -> err ("audio archive: " ^ message))
    | _ -> err "audio archive directory is not a directory"
