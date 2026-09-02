(* Builds the desired-state tree from a loaded Config.Stt.t *)

let build ?(min_duration_ms = 120) (config : Config.Stt.t) :
    Eta_component.Desired_state.t =
  let pulse_entry =
    Pulse_client.make_entry (Config_slices.pulse_client_slice config)
  in
  let keyboard_entry =
    Keyboard_monitor.make_entry (Config_slices.keyboard_monitor_slice config)
  in
  let recording_entry =
    Recording_coordinator.make_entry
      (Config_slices.recording_coordinator_slice config)
  in
  let stt_entry =
    Stt_backend.make_entry (Config_slices.stt_backend_slice config)
  in
  let wake_entry = Wake.make_entry (Config_slices.wakeword_slice config) in
  let rpc_entry =
    Rpc_server.make_entry (Config_slices.rpc_server_slice config)
  in
  let ptt_entry =
    Ptt.make_entry (Config_slices.ptt_loop_slice ~min_duration_ms config)
  in
  Eta_component.Desired_state.tree
    [
      Eta_component.Desired_state.component pulse_entry;
      Eta_component.Desired_state.component keyboard_entry;
      Eta_component.Desired_state.component recording_entry;
      Eta_component.Desired_state.component stt_entry;
      Eta_component.Desired_state.component wake_entry;
      Eta_component.Desired_state.component rpc_entry;
      Eta_component.Desired_state.component ptt_entry;
    ]

(* Expose entry IDs for diagnostics and supervision *)
let entry_ids =
  [
    "pulse-client";
    "keyboard-monitor";
    "recording-coordinator";
    "stt-backend";
    "wakeword";
    "rpc-server";
    "ptt-loop";
  ]

let is_infrastructure id = id = "pulse-client" || id = "keyboard-monitor"

(* Helper to get entry by id for replacement *)
let stt_entry_id = Stt_backend.entry_id
