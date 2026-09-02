(* Global env for component activations that need Eio resources.
   Set by Assistant.Service before Context.run, read by component activations. *)

let sw_ref : Eio.Switch.t option ref = ref None
let net_ref : Obj.t option ref = ref None
let clock_ref : Obj.t option ref = ref None
let process_mgr_ref : Obj.t option ref = ref None
let coordinator_ref : Recording.Coordinator.t option ref = ref None
let control_ref : Obj.t option ref = ref None
let audio_ref : Obj.t option ref = ref None
let keyboard_ref : Obj.t option ref = ref None
let set_sw v = sw_ref := Some v
let set_net v = net_ref := Some (Obj.repr v)
let set_clock v = clock_ref := Some (Obj.repr v)
let set_process_mgr v = process_mgr_ref := Some (Obj.repr v)
let set_coordinator v = coordinator_ref := Some v
let set_control v = control_ref := Some (Obj.repr v)
let set_audio v = audio_ref := Some (Obj.repr v)
let set_keyboard v = keyboard_ref := Some (Obj.repr v)

let get_sw () =
  match !sw_ref with Some v -> v | None -> failwith "Env.sw not set"

let get_net () : _ Eio.Net.t =
  match !net_ref with Some v -> Obj.obj v | None -> failwith "Env.net not set"

let get_clock () : _ Eio.Time.clock =
  match !clock_ref with
  | Some v -> Obj.obj v
  | None -> failwith "Env.clock not set"

let get_process_mgr () : _ Eio.Process.mgr =
  match !process_mgr_ref with
  | Some v -> Obj.obj v
  | None -> failwith "Env.process_mgr not set"

let get_coordinator () =
  match !coordinator_ref with
  | Some v -> v
  | None -> failwith "Env.coordinator not set"

let get_control () : Ptt_loop.trigger Eio.Stream.t =
  match !control_ref with
  | Some v -> Obj.obj v
  | None -> failwith "Env.control not set"

let get_audio () :
    (bytes, [ `Pulse_error of Pulse.Client.error ]) Eta_stream.Stream.t =
  match !audio_ref with
  | Some v -> Obj.obj v
  | None -> failwith "Env.audio not set"

let get_keyboard () : Keyboard.Monitor.key_event Eio.Stream.t =
  match !keyboard_ref with
  | Some v -> Obj.obj v
  | None -> failwith "Env.keyboard not set"
