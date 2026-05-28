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

let a11y_service = "org.freedesktop.a11y.Manager"
let a11y_path = "/org/freedesktop/a11y/Manager"
let a11y_interface = "org.freedesktop.a11y.KeyboardMonitor"

let dbus_service = "org.freedesktop.DBus"
let dbus_path = "/org/freedesktop/DBus"
let dbus_interface = "org.freedesktop.DBus"

let int_of_json = function
  | `Int n -> Some n
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let parse_busctl_json_event line =
  try
    match Yojson.Safe.from_string line with
    | `Assoc fields ->
      let field key = List.assoc_opt key fields in
      (match field "type", field "interface", field "member", field "payload" with
       | Some (`String "signal"),
         Some (`String iface),
         Some (`String "KeyEvent"),
         Some (`Assoc payload)
         when iface = a11y_interface ->
         (match List.assoc_opt "data" payload with
          | Some (`List [released; state; keysym; unichar; keycode]) ->
            (match released, int_of_json state, int_of_json keysym, int_of_json unichar, int_of_json keycode with
             | `Bool released, Some state, Some keysym, Some unichar, Some keycode ->
               Some { released; state; keysym; unichar; keycode }
             | _ -> None)
          | _ -> None)
       | _ -> None)
    | _ -> None
  with _ -> None

let percent_decode s =
  let hex = function
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
    | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
    | _ -> None
  in
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then Buffer.contents buf
    else if s.[i] = '%' && i + 2 < String.length s then
      match hex s.[i + 1], hex s.[i + 2] with
      | Some hi, Some lo ->
        Buffer.add_char buf (Char.chr ((hi lsl 4) lor lo));
        loop (i + 3)
      | _ ->
        Buffer.add_char buf s.[i];
        loop (i + 1)
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

let session_bus_path () =
  let address =
    match Sys.getenv_opt "DBUS_SESSION_BUS_ADDRESS" with
    | Some value -> value
    | None ->
      let runtime =
        match Sys.getenv_opt "XDG_RUNTIME_DIR" with
        | Some value -> value
        | None -> "/run/user/" ^ string_of_int (Unix.getuid ())
      in
      "unix:path=" ^ Filename.concat runtime "bus"
  in
  let first =
    match String.split_on_char ';' address with
    | hd :: _ -> hd
    | [] -> address
  in
  let prefix = "unix:path=" in
  if String.length first >= String.length prefix
     && String.sub first 0 (String.length prefix) = prefix then
    percent_decode
      (String.sub first (String.length prefix)
         (String.length first - String.length prefix))
  else
    invalid_arg ("unsupported DBUS_SESSION_BUS_ADDRESS: " ^ address)

let write flow s = Eio.Flow.write flow [Cstruct.of_string s]

let read_exact flow len =
  let buffer = Cstruct.create len in
  let rec loop off =
    if off = len then Cstruct.to_bytes buffer
    else
      let cs = Cstruct.sub buffer off (len - off) in
      match Eio.Flow.single_read flow cs with
      | 0 -> raise End_of_file
      | n -> loop (off + n)
  in
  loop 0

let read_crlf_line flow =
  let buf = Buffer.create 64 in
  let rec loop last_cr =
    let b = read_exact flow 1 in
    let c = Bytes.get b 0 in
    if last_cr && c = '\n' then Buffer.contents buf
    else (
      if last_cr then Buffer.add_char buf '\r';
      if c = '\r' then loop true
      else (
        Buffer.add_char buf c;
        loop false))
  in
  loop false

let put_u16 buf n =
  Buffer.add_char buf (Char.chr (n land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff))

let put_u32 buf n =
  Buffer.add_char buf (Char.chr (n land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))

let get_u16 bytes off =
  Char.code (Bytes.get bytes off)
  lor (Char.code (Bytes.get bytes (off + 1)) lsl 8)

let get_u32 bytes off =
  Char.code (Bytes.get bytes off)
  lor (Char.code (Bytes.get bytes (off + 1)) lsl 8)
  lor (Char.code (Bytes.get bytes (off + 2)) lsl 16)
  lor (Char.code (Bytes.get bytes (off + 3)) lsl 24)

let pad_len off alignment = (alignment - (off mod alignment)) mod alignment
let align off alignment = off + pad_len off alignment

let pad_buffer buf alignment =
  for _ = 1 to pad_len (Buffer.length buf) alignment do
    Buffer.add_char buf '\000'
  done

let add_dbus_string buf s =
  pad_buffer buf 4;
  put_u32 buf (String.length s);
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

let add_dbus_signature buf s =
  Buffer.add_char buf (Char.chr (String.length s));
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

let add_header_field buf code signature add_value =
  pad_buffer buf 8;
  Buffer.add_char buf (Char.chr code);
  add_dbus_signature buf signature;
  (match signature with
   | "s" | "o" | "u" -> pad_buffer buf 4
   | "q" -> pad_buffer buf 2
   | _ -> ());
  add_value buf

let add_string_field buf code value =
  add_header_field buf code "s" (fun buf -> add_dbus_string buf value)

let add_object_path_field buf code value =
  add_header_field buf code "o" (fun buf -> add_dbus_string buf value)

let add_signature_field buf code value =
  add_header_field buf code "g" (fun buf -> add_dbus_signature buf value)

let make_method_call ?signature ?body ~serial ~destination ~path ~interface ~member () =
  let fields = Buffer.create 128 in
  add_object_path_field fields 1 path;
  add_string_field fields 2 interface;
  add_string_field fields 3 member;
  add_string_field fields 6 destination;
  Option.iter (add_signature_field fields 8) signature;
  let fields = Buffer.contents fields in
  let body = Option.value ~default:"" body in
  let header = Buffer.create 128 in
  Buffer.add_char header 'l';
  Buffer.add_char header (Char.chr 1);
  Buffer.add_char header '\000';
  Buffer.add_char header (Char.chr 1);
  put_u32 header (String.length body);
  put_u32 header serial;
  put_u32 header (String.length fields);
  Buffer.add_string header fields;
  pad_buffer header 8;
  Buffer.contents header ^ body

type dbus_message = {
  msg_type : int;
  reply_serial : int option;
  interface : string option;
  member : string option;
  error_name : string option;
  body : bytes;
}

let read_dbus_string bytes off =
  let len = get_u32 bytes off in
  Bytes.sub_string bytes (off + 4) len, off + 4 + len + 1

let read_signature bytes off =
  let len = Char.code (Bytes.get bytes off) in
  Bytes.sub_string bytes (off + 1) len, off + 1 + len + 1

let parse_header_fields bytes len =
  let rec loop off reply_serial interface member error_name =
    if off >= len then reply_serial, interface, member, error_name
    else
      let off = align off 8 in
      if off >= len then reply_serial, interface, member, error_name
      else
        let code = Char.code (Bytes.get bytes off) in
        let signature, value_off = read_signature bytes (off + 1) in
        let value_off =
          match signature with
          | "s" | "o" | "u" -> align value_off 4
          | "q" -> align value_off 2
          | _ -> value_off
        in
        let next, reply_serial, interface, member, error_name =
          match signature with
          | "s" | "o" ->
            let value, next = read_dbus_string bytes value_off in
            let interface = if code = 2 then Some value else interface in
            let member = if code = 3 then Some value else member in
            let error_name = if code = 4 then Some value else error_name in
            next, reply_serial, interface, member, error_name
          | "u" ->
            let value = get_u32 bytes value_off in
            let reply_serial = if code = 5 then Some value else reply_serial in
            value_off + 4, reply_serial, interface, member, error_name
          | "q" ->
            value_off + 2, reply_serial, interface, member, error_name
          | "g" ->
            let _, next = read_signature bytes value_off in
            next, reply_serial, interface, member, error_name
          | _ ->
            value_off, reply_serial, interface, member, error_name
        in
        loop next reply_serial interface member error_name
  in
  loop 0 None None None None

let read_message flow =
  let fixed = read_exact flow 16 in
  if Bytes.get fixed 0 <> 'l' then failwith "unsupported D-Bus endianness";
  let msg_type = Char.code (Bytes.get fixed 1) in
  let body_len = get_u32 fixed 4 in
  let fields_len = get_u32 fixed 12 in
  let header_len = align (16 + fields_len) 8 in
  let rest = read_exact flow (header_len - 16 + body_len) in
  let fields = Bytes.sub rest 0 fields_len in
  let body = Bytes.sub rest (header_len - 16) body_len in
  let reply_serial, interface, member, error_name =
    parse_header_fields fields fields_len
  in
  { msg_type; reply_serial; interface; member; error_name; body }

let parse_key_event msg =
  match msg.msg_type, msg.interface, msg.member with
  | 4, Some iface, Some "KeyEvent" when iface = a11y_interface ->
    let body = msg.body in
    if Bytes.length body < 18 then None
    else
      let released = get_u32 body 0 <> 0 in
      let state = get_u32 body 4 in
      let keysym = get_u32 body 8 in
      let unichar = get_u32 body 12 in
      let keycode = get_u16 body 16 in
      Some { released; state; keysym; unichar; keycode }
  | _ -> None

let authenticate flow =
  let uid = string_of_int (Unix.getuid ()) in
  let hex_uid =
    String.concat ""
      (List.init (String.length uid)
         (fun i -> Printf.sprintf "%02x" (Char.code uid.[i])))
  in
  write flow ("\000AUTH EXTERNAL " ^ hex_uid ^ "\r\n");
  let line = read_crlf_line flow in
  if not (String.length line >= 2 && String.sub line 0 2 = "OK") then
    failwith ("D-Bus auth failed: " ^ line);
  write flow "BEGIN\r\n"

let call_method flow serial ~destination ~path ~interface ~member ?signature ?body () =
  write flow
    (make_method_call
       ?signature
       ?body
       ~serial
       ~destination
       ~path
       ~interface
       ~member
       ());
  let rec wait () =
    let msg = read_message flow in
    match msg.msg_type, msg.reply_serial with
    | 2, Some reply when reply = serial -> ()
    | 3, Some reply when reply = serial ->
      let name = Option.value ~default:"<unknown>" msg.error_name in
      failwith (member ^ " failed: " ^ name)
    | _ -> wait ()
  in
  wait ()

let dbus_string_body s =
  let buf = Buffer.create (String.length s + 8) in
  add_dbus_string buf s;
  Buffer.contents buf

let add_match flow serial =
  let rule =
    "type='signal',interface='" ^ a11y_interface ^ "',member='KeyEvent'"
  in
  call_method flow serial
    ~destination:dbus_service
    ~path:dbus_path
    ~interface:dbus_interface
    ~member:"AddMatch"
    ~signature:"s"
    ~body:(dbus_string_body rule)
    ()

let call_keyboard_method flow serial member =
  call_method flow serial
    ~destination:a11y_service
    ~path:a11y_path
    ~interface:a11y_interface
    ~member
    ()

let connect_and_watch ~sw ~net =
  let flow = Eio.Net.connect ~sw net (`Unix (session_bus_path ())) in
  authenticate flow;
  call_method flow 1
    ~destination:dbus_service
    ~path:dbus_path
    ~interface:dbus_interface
    ~member:"Hello"
    ();
  add_match flow 2;
  call_keyboard_method flow 3 "WatchKeyboard";
  flow

let unwatch_and_close flow =
  try
    write flow
      (make_method_call
         ~serial:4
         ~destination:a11y_service
         ~path:a11y_path
         ~interface:a11y_interface
         ~member:"UnwatchKeyboard"
         ());
    Eio.Flow.close flow
  with _ -> ()

let read_loop flow t =
  let rec loop () =
    let msg = read_message flow in
    match parse_key_event msg with
    | Some event ->
      emit t event;
      loop ()
    | None -> loop ()
  in
  try loop () with
  | Eio.Cancel.Cancelled _ -> ()
  | End_of_file -> ()
  | exn -> Printf.eprintf "keyboard monitor stopped: %s\n%!" (Printexc.to_string exn)

let with_monitor ~sw ~net ~process_mgr:_ f =
  Eta.Effect.acquire_release
    ~acquire:(Eta.Effect.sync (fun () ->
      let t = create () in
      let flow = connect_and_watch ~sw ~net in
      t, flow))
    ~release:(fun (_t, flow) -> Eta.Effect.sync (fun () -> unwatch_and_close flow))
  |> Eta.Effect.bind (fun (t, flow) ->
    Eta.Effect.with_background
      ~name:"keyboard.monitor"
      (Eta.Effect.sync (fun () -> read_loop flow t))
      (fun () -> f t))
