open Eta_component
open Eta.Syntax

let infra_ids = [
  Pulse_client.entry_id;
  Keyboard_monitor.entry_id;
]

let is_infra id =
  List.exists (fun infra -> Entry_id.equal infra id) infra_ids

let phase_is_failed = function
  | Diagnostics.Activation_failed _ -> true
  | _ -> false

let backoff_cap_ms = 30_000
let initial_backoff_ms = 1000

let next_backoff current =
  let doubled = current * 2 in
  if doubled > backoff_cap_ms then backoff_cap_ms else doubled

(* Supervision loop: watches diagnostics and retries failed infrastructure entries
   with bounded exponential backoff. Does not retry stt-backend. *)
let run ~context ~diagnostics =
  let backoffs = Hashtbl.create 4 in
  let get_backoff id =
    let key = Format.asprintf "%a" Entry_id.pp id in
    Option.value ~default:initial_backoff_ms (Hashtbl.find_opt backoffs key)
  in
  let set_backoff id v =
    let key = Format.asprintf "%a" Entry_id.pp id in
    Hashtbl.replace backoffs key v
  in
  let reset_backoff id =
    let key = Format.asprintf "%a" Entry_id.pp id in
    Hashtbl.remove backoffs key
  in
  let retry_one inst =
    let id = Diagnostics.entry_id inst in
    let backoff = get_backoff id in
    let* () = Eta.Effect.delay (Eta.Duration.ms backoff) Eta.Effect.unit in
    let* () =
      (Context.retry_and_await context id |> Eta.Effect.ignore_errors)
      |> Eta.Effect.ignore_errors
    in
    let* () = Eta.Effect.sync (fun () -> set_backoff id (next_backoff backoff)) |> Eta.Effect.ignore_errors in
    Eta.Effect.unit
  in
  let rec loop () =
    let* snapshot_opt =
      Diagnostics.snapshot diagnostics
      |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
    in
    match snapshot_opt with
    | None -> Eta.Effect.unit
    | Some snapshot ->
      let revision = Diagnostics.revision snapshot in
      let* change_opt =
        Diagnostics.await_change diagnostics ~after:revision
        |> Eta.Effect.fold ~ok:(fun c -> Some c) ~error:(fun _ -> None)
      in
      match change_opt with
      | None -> Eta.Effect.unit
      | Some (Diagnostics.Closed _) ->
        Eta.Effect.unit
      | Some (Diagnostics.Changed new_snapshot) ->
        let instances = Diagnostics.instances new_snapshot in
        List.iter (fun inst ->
          let id = Diagnostics.entry_id inst in
          if is_infra id then (
            match Diagnostics.phase inst with
            | Diagnostics.Active _ -> reset_backoff id
            | _ -> ()
          )
        ) instances;
        let failed_infra =
          List.filter (fun inst ->
            phase_is_failed (Diagnostics.phase inst)
            && is_infra (Diagnostics.entry_id inst)
          ) instances
        in
        let* () =
          failed_infra
          |> List.map retry_one
          |> Eta.Effect.concat
          |> Eta.Effect.ignore_errors
        in
        loop ()
  in
  loop ()
