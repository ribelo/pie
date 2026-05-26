type key_event = {
  released : bool;
  state : int;
  keysym : int;
  unichar : int;
  keycode : int;
}

type t = {
  queue : key_event Eio.Stream.t;
}

let create () =
  { queue = Eio.Stream.create 64 }

let subscribe t = t.queue

let emit t event =
  Eio.Stream.add t.queue event

let keysym_of_name = function
  | "F9" -> 65478
  | "F10" -> 65479
  | _ -> 0
