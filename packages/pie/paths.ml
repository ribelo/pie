let app_dir_name = "pie"

let resolve_xdg_home ~caps env_key fallback =
  match Ox_caps.Cap_sys.getenv_opt ~caps env_key with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
      let home =
        Option.value ~default:"/tmp" (Ox_caps.Cap_sys.getenv_opt ~caps "HOME")
      in
      Filename.concat home fallback

let xdg_data_home ~caps = resolve_xdg_home ~caps "XDG_DATA_HOME" ".local/share"
let xdg_config_home ~caps = resolve_xdg_home ~caps "XDG_CONFIG_HOME" ".config"

let xdg_runtime_dir ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "XDG_RUNTIME_DIR" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> (
      match Ox_caps.Cap_sys.getenv_opt ~caps "HOME" with
      | Some h -> Filename.concat h ".tmp"
      | None -> "/tmp")

let resolve_runtime_dir ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "CAMELPIE_RUNTIME_DIR" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> Filename.concat (xdg_runtime_dir ~caps) app_dir_name

let data_dir ~caps = Filename.concat (xdg_data_home ~caps) app_dir_name
let config_dir ~caps = Filename.concat (xdg_config_home ~caps) app_dir_name
let runtime_dir ~caps = resolve_runtime_dir ~caps

let daemon_socket_path ~caps =
  Filename.concat (runtime_dir ~caps) "control.sock"

let recording_state_path ~caps =
  Filename.concat (runtime_dir ~caps) "recording.json"

let failed_recordings_dir ~caps =
  Filename.concat (data_dir ~caps) "failed-recordings"

let stt_config_path ~caps =
  Ox_app.Config.path_with ~file_name:"stt.toml" ~name:app_dir_name ~config:None
    ~getenv:(Ox_caps.Cap_sys.getenv_opt ~caps)

let prompts_dir ~caps = Filename.concat (config_dir ~caps) "prompts"
