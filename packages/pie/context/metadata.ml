type entry = {
  manifest : string;
  language : string;
  package : string option;
  description : string option;
}

let trim_nonempty value =
  let value = String.trim value in
  if value = "" then None else Some value

let file_in_dir dir name = Filename.concat dir name
let regular_file path = Sys.file_exists path && not (Sys.is_directory path)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let json_string fields key =
  match List.assoc_opt key fields with
  | Some (`String value) -> trim_nonempty value
  | _ -> None

let package_json dir =
  let manifest = "package.json" in
  let path = file_in_dir dir manifest in
  if not (regular_file path) then []
  else
    try
      match Yojson.Safe.from_file path with
      | `Assoc fields ->
          [
            {
              manifest;
              language = "TypeScript";
              package = json_string fields "name";
              description = json_string fields "description";
            };
          ]
      | _ -> []
    with _ -> []

module TT = Toml.Types

let toml_key key = TT.Table.Key.of_string key
let toml_find table key = TT.Table.find_opt (toml_key key) table

let toml_string table key =
  match toml_find table key with
  | Some (TT.TString value) -> trim_nonempty value
  | _ -> None

let toml_table table key =
  match toml_find table key with
  | Some (TT.TTable table) -> Some table
  | _ -> None

let parse_toml path =
  try
    match Toml.Parser.from_filename path with
    | `Ok table -> Some table
    | `Error _ -> None
  with Sys_error _ -> None

let cargo_toml dir =
  let manifest = "Cargo.toml" in
  let path = file_in_dir dir manifest in
  if not (regular_file path) then []
  else
    match parse_toml path with
    | None -> []
    | Some table ->
        let package_table = toml_table table "package" in
        [
          {
            manifest;
            language = "Rust";
            package =
              Option.bind package_table (fun table -> toml_string table "name");
            description =
              Option.bind package_table (fun table ->
                  toml_string table "description");
          };
        ]

let pyproject_toml dir =
  let manifest = "pyproject.toml" in
  let path = file_in_dir dir manifest in
  if not (regular_file path) then []
  else
    match parse_toml path with
    | None -> []
    | Some table ->
        let project_table = toml_table table "project" in
        [
          {
            manifest;
            language = "Python";
            package =
              Option.bind project_table (fun table -> toml_string table "name");
            description =
              Option.bind project_table (fun table ->
                  toml_string table "description");
          };
        ]

type sexp = Atom of string | List of sexp list
type token = Lparen | Rparen | TAtom of string

let sexp_tokens input =
  let len = String.length input in
  let buf = Buffer.create 32 in
  let tokens = ref [] in
  let push_token token = tokens := token :: !tokens in
  let push_atom () =
    if Buffer.length buf > 0 then (
      push_token (TAtom (Buffer.contents buf));
      Buffer.clear buf)
  in
  let rec skip_comment i =
    if i >= len then i
    else if input.[i] = '\n' then i + 1
    else skip_comment (i + 1)
  in
  let rec read_string i =
    if i >= len then i
    else
      match input.[i] with
      | '"' -> i + 1
      | '\\' when i + 1 < len ->
          Buffer.add_char buf
            (match input.[i + 1] with
            | 'n' -> '\n'
            | 't' -> '	'
            | 'r' -> '\n'
            | 'b' -> ''
            | c -> c);
          read_string (i + 2)
      | c ->
          Buffer.add_char buf c;
          read_string (i + 1)
  in
  let rec loop i =
    if i >= len then push_atom ()
    else
      match input.[i] with
      | '(' ->
          push_atom ();
          push_token Lparen;
          loop (i + 1)
      | ')' ->
          push_atom ();
          push_token Rparen;
          loop (i + 1)
      | '"' ->
          push_atom ();
          let next = read_string (i + 1) in
          push_atom ();
          loop next
      | ';' ->
          push_atom ();
          loop (skip_comment (i + 1))
      | ' ' | '\n' | '\r' | '\t' ->
          push_atom ();
          loop (i + 1)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0;
  List.rev !tokens

let parse_sexps tokens =
  let rec parse_one = function
    | [] -> None
    | TAtom atom :: rest -> Some (Atom atom, rest)
    | Rparen :: _ -> None
    | Lparen :: rest ->
        let rec parse_items acc = function
          | [] -> None
          | Rparen :: rest -> Some (List (List.rev acc), rest)
          | tokens -> (
              match parse_one tokens with
              | None -> None
              | Some (sexp, rest) -> parse_items (sexp :: acc) rest)
        in
        parse_items [] rest
  in
  let rec loop acc = function
    | [] -> Some (List.rev acc)
    | tokens -> (
        match parse_one tokens with
        | None -> None
        | Some (sexp, rest) -> loop (sexp :: acc) rest)
  in
  loop [] tokens

let form name = function
  | List (Atom head :: values) when head = name -> Some values
  | _ -> None

let first_atom = function Atom value :: _ -> trim_nonempty value | _ -> None
let find_form name forms = forms |> List.find_map (form name)
let sexp_field name forms = Option.bind (find_form name forms) first_atom

let dune_project dir =
  let manifest = "dune-project" in
  let path = file_in_dir dir manifest in
  if not (regular_file path) then []
  else
    try
      let forms = read_file path |> sexp_tokens |> parse_sexps in
      match forms with
      | None -> []
      | Some forms ->
          let package_forms =
            Option.value ~default:[] (find_form "package" forms)
          in
          [
            {
              manifest;
              language = "OCaml";
              package =
                (match sexp_field "name" package_forms with
                | Some name -> Some name
                | None -> sexp_field "name" forms);
              description =
                (match sexp_field "description" package_forms with
                | Some description -> Some description
                | None -> sexp_field "synopsis" package_forms);
            };
          ]
    with _ -> []

let opam_field name content =
  let prefix = name ^ ":" in
  content |> String.split_on_char '\n'
  |> List.find_map (fun line ->
      let line = String.trim line in
      if String.length line <= String.length prefix then None
      else if String.sub line 0 (String.length prefix) <> prefix then None
      else
        let value =
          String.sub line (String.length prefix)
            (String.length line - String.length prefix)
          |> String.trim
        in
        let value =
          if
            String.length value >= 2
            && value.[0] = '"'
            && value.[String.length value - 1] = '"'
          then String.sub value 1 (String.length value - 2)
          else value
        in
        trim_nonempty value)

let opam_files dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".opam")
    |> List.sort String.compare
  with Sys_error _ -> []

let opam dir =
  if regular_file (file_in_dir dir "dune-project") then []
  else
    opam_files dir
    |> List.filter_map (fun manifest ->
        let path = file_in_dir dir manifest in
        if not (regular_file path) then None
        else
          try
            let content = read_file path in
            Some
              {
                manifest;
                language = "OCaml";
                package = Some (Filename.chop_suffix manifest ".opam");
                description =
                  (match opam_field "description" content with
                  | Some description -> Some description
                  | None -> opam_field "synopsis" content);
              }
          with Sys_error _ -> None)

let load_for_cwd dir =
  package_json dir @ cargo_toml dir @ pyproject_toml dir @ dune_project dir
  @ opam dir

let entry_lines entry =
  [
    Some ("- manifest: " ^ entry.manifest);
    Some ("- language: " ^ entry.language);
    Option.map (fun package -> "- package: " ^ package) entry.package;
    Option.map
      (fun description -> "- description: " ^ description)
      entry.description;
  ]
  |> List.filter_map Fun.id

let append_to_prompt prompt_template entries =
  match entries with
  | [] -> prompt_template
  | entries ->
      prompt_template
      ^ "\n\n\
         Project metadata for transcription and translation disambiguation:\n"
      ^ "Use this metadata to choose words, spellings, identifiers, and \
         technical terms that best fit the project context. Do not mention \
         this metadata in the output unless it is actually spoken or needed \
         for the requested translation.\n"
      ^ (entries |> List.map entry_lines
        |> List.map (String.concat "\n")
        |> String.concat "\n\n")
