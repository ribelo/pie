type error = [ `Niri_error of string ]

let run_niri_json ~process_mgr args =
  (match args with
    | "msg" :: rest ->
        Sh.run_capture ~process_mgr ~prog:"niri"
          ~args:("msg" :: "--json" :: rest)
    | _ -> Sh.run_capture ~process_mgr ~prog:"niri" ~args)
  |> Result.map_error (fun (`Subprocess_error msg) ->
      `Niri_error ("niri command failed: " ^ msg))

type window_info = {
  id : int;
  title : string option;
  app_id : string option;
  pid : int option;
  workspace_id : int option;
}

let focused_window ~process_mgr () =
  let open Result.Syntax in
  let* output = run_niri_json ~process_mgr [ "msg"; "focused-window" ] in
  try
    Ok
      (let open Yojson.Safe in
       let json = from_string output in
       match json with
       | `Null -> None
       | `Assoc fields ->
           let find_int key =
             match List.assoc_opt key fields with
             | Some (`Int i) -> Some i
             | _ -> None
           in
           let find_string key =
             match List.assoc_opt key fields with
             | Some (`String s) -> Some s
             | _ -> None
           in
           Some
             {
               id = Option.value (find_int "id") ~default:0;
               title = find_string "title";
               app_id = find_string "app_id";
               pid = find_int "pid";
               workspace_id = find_int "workspace_id";
             }
       | _ -> None)
  with Yojson.Json_error _ ->
    Error (`Niri_error "focused-window json invalid")
