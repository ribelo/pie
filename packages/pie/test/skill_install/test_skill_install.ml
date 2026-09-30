let pie_exe =
  let raw =
    Filename.concat (Filename.dirname Sys.executable_name) "../../bin/pie.exe"
  in
  Unix.realpath raw

let child_deadline_s = 8.0

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let rec mkdir_p path =
  if path = "" || path = "/" || Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o700)

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stats when stats.Unix.st_kind = Unix.S_LNK -> Unix.unlink path
  | stats when stats.Unix.st_kind = Unix.S_DIR ->
      Array.iter
        (fun name -> rm_rf (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp name f =
  let path = Filename.temp_file name "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> rm_rf path) (fun () -> f path)

let with_cwd dir f =
  let previous = Sys.getcwd () in
  Unix.chdir dir;
  Fun.protect ~finally:(fun () -> Unix.chdir previous) f

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let read_available buf fd =
  let bytes = Bytes.create 256 in
  try
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> `Eof
    | n ->
        Buffer.add_subbytes buf bytes 0 n;
        `Read
  with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> `Empty
  | Unix.Unix_error (Unix.EBADF, _, _) -> `Eof

let rec drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
    stdout_open stderr_open =
  if not (stdout_open || stderr_open) then (false, false)
  else
    let now = Unix.gettimeofday () in
    if now >= deadline then (stdout_open, stderr_open)
    else
      let timeout = min 0.05 (deadline -. now) in
      let waiting =
        (if stdout_open then [ stdout_r ] else [])
        @ if stderr_open then [ stderr_r ] else []
      in
      let ready, _, _ =
        try Unix.select waiting [] [] timeout
        with Unix.Unix_error (Unix.EBADF, _, _) -> ([], [], [])
      in
      let stdout_open, stdout_more =
        if List.mem stdout_r ready then
          match read_available stdout_buf stdout_r with
          | `Eof ->
              close_fd stdout_r;
              (false, false)
          | `Read -> (true, true)
          | `Empty -> (true, false)
        else (stdout_open, false)
      in
      let stderr_open, stderr_more =
        if List.mem stderr_r ready then
          match read_available stderr_buf stderr_r with
          | `Eof ->
              close_fd stderr_r;
              (false, false)
          | `Read -> (true, true)
          | `Empty -> (true, false)
        else (stderr_open, false)
      in
      if stdout_more || stderr_more then
        drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
          stdout_open stderr_open
      else (stdout_open, stderr_open)

type cli_run = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let inherited name =
  match Sys.getenv_opt name with
  | None -> []
  | Some value -> [ name ^ "=" ^ value ]

let child_env ~sandbox ~home =
  let xdg = Filename.concat sandbox "xdg" in
  mkdir_p xdg;
  let home_binding =
    match home with None -> [] | Some home -> [ "HOME=" ^ home ]
  in
  Array.of_list
    ([
       "PATH=" ^ Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH");
       "LC_ALL=C";
       "OTEL_SDK_DISABLED=true";
       "XDG_CONFIG_HOME=" ^ xdg;
       "XDG_DATA_HOME=" ^ xdg;
       "XDG_CACHE_HOME=" ^ xdg;
       "XDG_RUNTIME_DIR=" ^ xdg;
     ]
    @ home_binding
    @ inherited "CAML_LD_LIBRARY_PATH"
    @ inherited "LD_LIBRARY_PATH")

let run_pie ~home ?(cwd = Sys.getcwd ()) ~sandbox args =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let stdin_r = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  Unix.set_nonblock stdout_r;
  Unix.set_nonblock stderr_r;
  let env = child_env ~sandbox ~home in
  let pid =
    with_cwd cwd (fun () ->
        Unix.create_process_env pie_exe
          (Array.of_list (pie_exe :: args))
          env stdin_r stdout_w stderr_w)
  in
  close_fd stdin_r;
  close_fd stdout_w;
  close_fd stderr_w;
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  let deadline = Unix.gettimeofday () +. child_deadline_s in
  let rec wait_child stdout_open stderr_open =
    let stdout_open, stderr_open =
      drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
        stdout_open stderr_open
    in
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then (
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          snd (Unix.waitpid [] pid))
        else wait_child stdout_open stderr_open
    | _, status ->
        ignore
          (drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
             stdout_open stderr_open);
        status
  in
  let status =
    Fun.protect
      ~finally:(fun () ->
        close_fd stdout_r;
        close_fd stderr_r)
      (fun () -> wait_child true true)
  in
  {
    status;
    stdout = Buffer.contents stdout_buf;
    stderr = Buffer.contents stderr_buf;
  }

let exit_code = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> Alcotest.failf "signaled %d" n
  | Unix.WSTOPPED n -> Alcotest.failf "stopped %d" n

let require_exit expected run =
  let code = exit_code run.status in
  if code <> expected then
    Alcotest.failf "expected exit %d, got %d\nstdout:\n%s\nstderr:\n%s" expected
      code run.stdout run.stderr

let field name = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some (`String value) -> value
      | _ -> Alcotest.failf "missing string field %s" name)
  | _ -> Alcotest.fail "expected JSON object"

let compact_outcomes text =
  String.split_on_char '\n' text
  |> List.filter (fun line -> line <> "")
  |> List.map (fun line ->
      if string_contains line "\n" then
        Alcotest.failf "compact JSON contains a newline: %s" line;
      let json = Yojson.Safe.from_string line in
      (field "action" json, field "path" json))

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel -> output_string channel contents)

let inode path = (Unix.stat path).st_ino
let skill_path root = Filename.concat root "pie-glossary"

let assert_contains path needles =
  let text = read_file path in
  List.iter
    (fun needle ->
      if not (string_contains text needle) then
        Alcotest.failf "%s lacks %S" path needle)
    needles

let skill_contract_needles =
  [
    ".pie/glossary.md";
    "{{glossary}}";
    "PIE_GLOSSARY";
    "glossary.md";
    "Eio — ejo, e i o";
    "OpenRouter — open router, open routera";
    "Nema — NEMa, nema";
    "next key press";
    "Do not delete";
    "Not every identifier in the code";
    "README";
    "CONTEXT.md";
    "Polish";
    "git log";
    "bin/";
  ]

let test_explicit_fresh () =
  with_temp "pie-skill-fresh" @@ fun sandbox ->
  let root = Filename.concat sandbox "skills" in
  mkdir_p root;
  let run =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--no-input"; root ]
  in
  require_exit 0 run;
  Alcotest.(check string) "stderr empty" "" run.stderr;
  let destination = skill_path root in
  Alcotest.(check (list (pair string string)))
    "installed json"
    [ ("installed", destination) ]
    (compact_outcomes run.stdout);
  assert_contains
    (Filename.concat destination "SKILL.md")
    skill_contract_needles;
  assert_contains
    (Filename.concat destination "references/examples.md")
    [ "Eio — ejo, e i o"; "Front Servera" ]

let test_current () =
  with_temp "pie-skill-current" @@ fun sandbox ->
  let root = Filename.concat sandbox "skills" in
  mkdir_p root;
  let first = run_pie ~sandbox ~home:None [ "skill"; "install"; root ] in
  require_exit 0 first;
  let destination = skill_path root in
  let skill_md = Filename.concat destination "SKILL.md" in
  let before = inode skill_md in
  let second = run_pie ~sandbox ~home:None [ "skill"; "install"; root ] in
  require_exit 0 second;
  Alcotest.(check (list (pair string string)))
    "current json"
    [ ("current", destination) ]
    (compact_outcomes second.stdout);
  Alcotest.(check int) "inode unchanged" before (inode skill_md);
  let dry =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--dry-run"; root ]
  in
  require_exit 0 dry;
  Alcotest.(check (list (pair string string)))
    "dry-run current"
    [ ("current", destination) ]
    (compact_outcomes dry.stdout);
  Alcotest.(check int) "dry-run keeps inode" before (inode skill_md)

let test_replacement_requires_consent () =
  with_temp "pie-skill-replace" @@ fun sandbox ->
  let root = Filename.concat sandbox "skills" in
  mkdir_p root;
  let installed = run_pie ~sandbox ~home:None [ "skill"; "install"; root ] in
  require_exit 0 installed;
  let destination = skill_path root in
  let skill_md = Filename.concat destination "SKILL.md" in
  let extra = Filename.concat destination "extra.txt" in
  let nested = Filename.concat destination "references/local-note.md" in
  write_file skill_md "stale";
  write_file extra "keep-me";
  write_file nested "local";
  let before = inode skill_md in
  let refused = run_pie ~sandbox ~home:None [ "skill"; "install"; root ] in
  require_exit 64 refused;
  if not (string_contains refused.stderr "rerun with --force") then
    Alcotest.failf "stderr lacks --force hint:\n%s" refused.stderr;
  Alcotest.(check string) "stdout empty" "" refused.stdout;
  Alcotest.(check string) "stale kept" "stale" (read_file skill_md);
  Alcotest.(check bool) "extra kept" true (Sys.file_exists extra);
  let no_input =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--no-input"; root ]
  in
  require_exit 64 no_input;
  Alcotest.(check string) "still stale" "stale" (read_file skill_md);
  Alcotest.(check int) "refused install keeps inode" before (inode skill_md);
  let forced =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--force"; root ]
  in
  require_exit 0 forced;
  Alcotest.(check (list (pair string string)))
    "replaced json"
    [ ("replaced", destination) ]
    (compact_outcomes forced.stdout);
  Alcotest.(check bool) "extra removed" false (Sys.file_exists extra);
  Alcotest.(check bool) "nested extra removed" false (Sys.file_exists nested);
  if read_file skill_md = "stale" then Alcotest.fail "force left stale skill";
  assert_contains skill_md [ ".pie/glossary.md"; "Eio — ejo, e i o" ];
  Alcotest.(check bool)
    "bundled reference restored" true
    (Sys.file_exists (Filename.concat destination "references/examples.md"))

let test_dry_run_changes_nothing () =
  with_temp "pie-skill-dry" @@ fun sandbox ->
  let root = Filename.concat sandbox "skills" in
  mkdir_p root;
  let fresh =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--dry-run"; root ]
  in
  require_exit 0 fresh;
  let destination = skill_path root in
  Alcotest.(check (list (pair string string)))
    "would install"
    [ ("would install", destination) ]
    (compact_outcomes fresh.stdout);
  Alcotest.(check bool) "nothing created" false (Sys.file_exists destination);
  ignore (run_pie ~sandbox ~home:None [ "skill"; "install"; "--force"; root ]);
  write_file (Filename.concat destination "SKILL.md") "stale";
  write_file (Filename.concat destination "extra.txt") "extra";
  let planned =
    run_pie ~sandbox ~home:None
      [ "skill"; "install"; "--dry-run"; "--force"; root ]
  in
  require_exit 0 planned;
  Alcotest.(check (list (pair string string)))
    "would replace"
    [ ("would replace", destination) ]
    (compact_outcomes planned.stdout);
  Alcotest.(check string)
    "dry-run keeps stale" "stale"
    (read_file (Filename.concat destination "SKILL.md"));
  Alcotest.(check bool)
    "dry-run keeps extra" true
    (Sys.file_exists (Filename.concat destination "extra.txt"))

let test_discovers_existing_roots_only () =
  with_temp "pie-skill-discover" @@ fun sandbox ->
  let home = Filename.concat sandbox "home" in
  mkdir_p (Filename.concat home ".claude");
  mkdir_p (Filename.concat home ".pi");
  let run =
    run_pie ~sandbox ~home:(Some home) [ "skill"; "install"; "--force" ]
  in
  require_exit 0 run;
  let claude = Filename.concat home ".claude/skills/pie-glossary" in
  let pi = Filename.concat home ".pi/agent/skills/pie-glossary" in
  Alcotest.(check (list (pair string string)))
    "two discovered targets"
    [ ("installed", claude); ("installed", pi) ]
    (compact_outcomes run.stdout);
  Alcotest.(check bool)
    "claude skill" true
    (Sys.file_exists (Filename.concat claude "SKILL.md"));
  Alcotest.(check bool)
    "pi skill" true
    (Sys.file_exists (Filename.concat pi "SKILL.md"));
  Alcotest.(check bool)
    "codex not created" false
    (Sys.file_exists (Filename.concat home ".codex"));
  Alcotest.(check bool)
    "agents not created" false
    (Sys.file_exists (Filename.concat home ".agents"))

let test_deduplicates_symlinked_root () =
  with_temp "pie-skill-dedup" @@ fun sandbox ->
  let home = Filename.concat sandbox "home" in
  let shared = Filename.concat sandbox "shared" in
  mkdir_p shared;
  mkdir_p (Filename.concat home ".claude");
  mkdir_p (Filename.concat home ".codex");
  Unix.symlink shared (Filename.concat home ".claude/skills");
  Unix.symlink shared (Filename.concat home ".codex/skills");
  let dry =
    run_pie ~sandbox ~home:(Some home) [ "skill"; "install"; "--dry-run" ]
  in
  require_exit 0 dry;
  let reported = Filename.concat home ".claude/skills/pie-glossary" in
  Alcotest.(check (list (pair string string)))
    "one planned target"
    [ ("would install", reported) ]
    (compact_outcomes dry.stdout);
  Alcotest.(check bool)
    "dry-run wrote nothing" false
    (Sys.file_exists (Filename.concat shared "pie-glossary"));
  let run =
    run_pie ~sandbox ~home:(Some home) [ "skill"; "install"; "--force" ]
  in
  require_exit 0 run;
  Alcotest.(check (list (pair string string)))
    "one installed target"
    [ ("installed", reported) ]
    (compact_outcomes run.stdout);
  let installed = Filename.concat shared "pie-glossary/SKILL.md" in
  Alcotest.(check bool) "shared skill" true (Sys.file_exists installed);
  Alcotest.(check int)
    "both links are one file" (inode installed)
    (inode (Filename.concat home ".codex/skills/pie-glossary/SKILL.md"))

let test_no_roots_lists_checked_locations () =
  with_temp "pie-skill-none" @@ fun sandbox ->
  let home = Filename.concat sandbox "home" in
  mkdir_p home;
  let run =
    run_pie ~sandbox ~home:(Some home) [ "skill"; "install"; "--force" ]
  in
  require_exit 64 run;
  Alcotest.(check string) "no json" "" run.stdout;
  List.iter
    (fun name ->
      let checked = Filename.concat home name in
      if not (string_contains run.stderr checked) then
        Alcotest.failf "stderr lacks %s:\n%s" checked run.stderr)
    [ ".claude"; ".codex"; ".pi"; ".agents" ];
  if not (string_contains run.stderr "no supported agent configuration found")
  then Alcotest.failf "stderr lacks discovery error:\n%s" run.stderr

let test_empty_path_is_usage () =
  with_temp "pie-skill-empty" @@ fun sandbox ->
  let cwd = Filename.concat sandbox "cwd" in
  mkdir_p cwd;
  let run =
    run_pie ~sandbox ~home:None ~cwd [ "skill"; "install"; "--force"; "" ]
  in
  require_exit 64 run;
  if not (string_contains run.stderr "PATH must not be empty") then
    Alcotest.failf "stderr lacks empty-path error:\n%s" run.stderr;
  Alcotest.(check bool)
    "cwd untouched" false
    (Sys.file_exists (Filename.concat cwd "pie-glossary"))

let test_json_per_target_and_pretty () =
  with_temp "pie-skill-json" @@ fun sandbox ->
  let home = Filename.concat sandbox "home" in
  mkdir_p (Filename.concat home ".claude");
  mkdir_p (Filename.concat home ".agents");
  let run =
    run_pie ~sandbox ~home:(Some home) [ "skill"; "install"; "--force" ]
  in
  require_exit 0 run;
  let outcomes = compact_outcomes run.stdout in
  Alcotest.(check int) "one object per target" 2 (List.length outcomes);
  List.iter
    (fun (action, path) ->
      Alcotest.(check string) "installed" "installed" action;
      Alcotest.(check bool)
        "path names skill" true
        (string_contains path "/pie-glossary");
      if string_contains run.stdout "\n  " then
        Alcotest.fail "compact output is indented")
    outcomes;
  let root = Filename.concat sandbox "explicit" in
  mkdir_p root;
  let pretty =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--pretty"; root ]
  in
  require_exit 0 pretty;
  if not (string_contains pretty.stdout "\n") then
    Alcotest.fail "pretty output is one line";
  let json = Yojson.Safe.from_string pretty.stdout in
  Alcotest.(check string) "pretty action" "installed" (field "action" json);
  Alcotest.(check string) "pretty path" (skill_path root) (field "path" json)

let test_filesystem_error_is_70 () =
  with_temp "pie-skill-fs" @@ fun sandbox ->
  let bad = Filename.concat sandbox "not-a-directory" in
  write_file bad "file";
  let run =
    run_pie ~sandbox ~home:None [ "skill"; "install"; "--force"; bad ]
  in
  require_exit 70 run;
  let destination = skill_path bad in
  if not (string_contains run.stderr destination) then
    Alcotest.failf "stderr lacks %s:\n%s" destination run.stderr;
  Alcotest.(check string) "file unchanged" "file" (read_file bad)

let test_discovered_target_needs_consent () =
  with_temp "pie-skill-consent" @@ fun sandbox ->
  let home = Filename.concat sandbox "home" in
  mkdir_p (Filename.concat home ".claude");
  let run = run_pie ~sandbox ~home:(Some home) [ "skill"; "install" ] in
  require_exit 64 run;
  if not (string_contains run.stderr "rerun with --force") then
    Alcotest.failf "stderr lacks --force hint:\n%s" run.stderr;
  Alcotest.(check bool)
    "not installed" false
    (Sys.file_exists (Filename.concat home ".claude/skills/pie-glossary"))

let test_relative_home_rejected () =
  with_temp "pie-skill-home" @@ fun sandbox ->
  let run =
    run_pie ~sandbox ~home:(Some "relative-home") [ "skill"; "install" ]
  in
  require_exit 64 run;
  if not (string_contains run.stderr "HOME must be an absolute path") then
    Alcotest.failf "stderr lacks HOME error:\n%s" run.stderr

let test_help () =
  with_temp "pie-skill-help" @@ fun sandbox ->
  let run = run_pie ~sandbox ~home:None [ "skill"; "install"; "--help" ] in
  require_exit 0 run;
  List.iter
    (fun needle ->
      if not (string_contains run.stdout needle) then
        Alcotest.failf "help lacks %S:\n%s" needle run.stdout)
    [
      "pie-glossary";
      "--force";
      "--dry-run";
      "--no-input";
      "--pretty";
      "pie skill install";
    ]

let () =
  Alcotest.run "skill install"
    [
      ( "skill install",
        [
          Alcotest.test_case "explicit path fresh install dict-a52r dict-5w57"
            `Quick test_explicit_fresh;
          Alcotest.test_case "unchanged install is current dict-pguq" `Quick
            test_current;
          Alcotest.test_case
            "changed skill requires consent and force removes extras dict-pguq"
            `Quick test_replacement_requires_consent;
          Alcotest.test_case "dry-run changes nothing dict-pguq" `Quick
            test_dry_run_changes_nothing;
          Alcotest.test_case "discovery uses existing roots only dict-a52r"
            `Quick test_discovers_existing_roots_only;
          Alcotest.test_case "symlinked roots are installed once dict-a52r"
            `Quick test_deduplicates_symlinked_root;
          Alcotest.test_case "no roots lists checked locations dict-a52r" `Quick
            test_no_roots_lists_checked_locations;
          Alcotest.test_case "empty PATH exits 64 dict-a52r" `Quick
            test_empty_path_is_usage;
          Alcotest.test_case "one JSON object per target dict-pguq" `Quick
            test_json_per_target_and_pretty;
          Alcotest.test_case "filesystem error exits 70 dict-a52r" `Quick
            test_filesystem_error_is_70;
          Alcotest.test_case
            "discovered target does not wait without a tty dict-pguq" `Quick
            test_discovered_target_needs_consent;
          Alcotest.test_case "relative HOME is rejected dict-a52r" `Quick
            test_relative_home_rejected;
          Alcotest.test_case "install help names flags and examples dict-a52r"
            `Quick test_help;
        ] );
    ]
