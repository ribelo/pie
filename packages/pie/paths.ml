let app_dir_name = "pie"

let resolve_xdg_home env_key fallback =
  match Sys.getenv_opt env_key with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
      let home = Option.value ~default:"/tmp" (Sys.getenv_opt "HOME") in
      Filename.concat home fallback

let xdg_data_home = resolve_xdg_home "XDG_DATA_HOME" ".local/share"
let xdg_config_home = resolve_xdg_home "XDG_CONFIG_HOME" ".config"

let resolve_xdg_runtime_dir () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> (
      match Sys.getenv_opt "HOME" with
      | Some h -> Filename.concat h ".tmp"
      | None -> "/tmp")

let xdg_runtime_dir = resolve_xdg_runtime_dir ()

let resolve_runtime_dir () =
  match Sys.getenv_opt "CAMELPIE_RUNTIME_DIR" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> Filename.concat xdg_runtime_dir app_dir_name

let data_dir = Filename.concat xdg_data_home app_dir_name
let config_dir = Filename.concat xdg_config_home app_dir_name
let runtime_dir = resolve_runtime_dir ()
let openwakeword_data_dir = Filename.concat data_dir "openwakeword"
let wakeword_config_dir = Filename.concat config_dir "wakeword"
let daemon_socket_path = Filename.concat runtime_dir "control.sock"
let recording_state_path = Filename.concat runtime_dir "recording.json"
let failed_recordings_dir = Filename.concat data_dir "failed-recordings"
let meetings_dir = Filename.concat data_dir "meetings"
let stt_config_path = Filename.concat config_dir "stt.toml"
let prompts_dir = Filename.concat config_dir "prompts"
