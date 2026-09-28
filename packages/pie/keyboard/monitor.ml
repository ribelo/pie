type key_event = {
  released : bool;
  state : int;
  keysym : int;
  unichar : int;
  keycode : int;
}

type t = { queue : key_event Eio.Stream.t }

exception Input_lost
exception Unsupported_input of string
exception Setup_timeout

type unix_endpoint = Filesystem of string | Abstract of string
type bus_address = { endpoint : unix_endpoint; guid : string option }
type budgets = { acquire : Eta.Duration.t; unwatch : Eta.Duration.t }

let production_budgets =
  { acquire = Eta.Duration.seconds 10; unwatch = Eta.Duration.ms 250 }

let max_auth_line_bytes = 4096
let max_message_bytes = 65536
let create () = { queue = Eio.Stream.create 64 }
let subscribe t = t.queue
let emit t event = Eio.Stream.add t.queue event

let keysym_of_name = function
  | "F1" -> 65470
  | "F2" -> 65471
  | "F3" -> 65472
  | "F4" -> 65473
  | "F5" -> 65474
  | "F6" -> 65475
  | "F7" -> 65476
  | "F8" -> 65477
  | "F9" -> 65478
  | "F10" -> 65479
  | "F11" -> 65480
  | "F12" -> 65481
  | name ->
      invalid_arg (Printf.sprintf "keysym_of_name: unknown key name %S" name)

let a11y_service = "org.freedesktop.a11y.Manager"
let a11y_path = "/org/freedesktop/a11y/Manager"
let a11y_interface = "org.freedesktop.a11y.KeyboardMonitor"
let dbus_service = "org.freedesktop.DBus"
let dbus_path = "/org/freedesktop/DBus"
let dbus_interface = "org.freedesktop.DBus"

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
    else if s.[i] = '%' then (
      if i + 2 >= String.length s then
        raise (Unsupported_input "invalid percent-encoding in bus address");
      match (hex s.[i + 1], hex s.[i + 2]) with
      | Some hi, Some lo ->
          Buffer.add_char buf (Char.chr ((hi lsl 4) lor lo));
          loop (i + 3)
      | _ -> raise (Unsupported_input "invalid percent-encoding in bus address"))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

let split_options rest =
  let rec loop i start acc =
    if i >= String.length rest then
      List.rev
        (if start < String.length rest then
           String.sub rest start (String.length rest - start) :: acc
         else if start = String.length rest && start > 0 then "" :: acc
         else acc)
    else if rest.[i] = ',' then
      loop (i + 1) (i + 1) (String.sub rest start (i - start) :: acc)
    else loop (i + 1) start acc
  in
  if rest = "" then [] else loop 0 0 []

let parse_option chunk =
  match String.index_opt chunk '=' with
  | None -> raise (Unsupported_input ("malformed unix address option " ^ chunk))
  | Some eq ->
      ( String.sub chunk 0 eq,
        percent_decode
          (String.sub chunk (eq + 1) (String.length chunk - eq - 1)) )

let parse_bus_address address =
  let first =
    match String.split_on_char ';' address with
    | hd :: _ when hd <> "" -> hd
    | _ -> raise (Unsupported_input "empty D-Bus address")
  in
  match String.index_opt first ':' with
  | None -> raise (Unsupported_input ("unsupported D-Bus address " ^ address))
  | Some colon -> (
      let transport = String.sub first 0 colon in
      let rest =
        String.sub first (colon + 1) (String.length first - colon - 1)
      in
      if transport <> "unix" then
        raise (Unsupported_input ("unsupported D-Bus transport " ^ transport));
      let path = ref None in
      let abstract = ref None in
      let guid = ref None in
      List.iter
        (fun chunk ->
          if chunk = "" then
            raise (Unsupported_input "empty unix address option");
          let key, value = parse_option chunk in
          match key with
          | "path" -> (
              match !path with
              | Some _ ->
                  raise (Unsupported_input "duplicate unix address option path")
              | None ->
                  if value = "" then
                    raise (Unsupported_input "empty unix path endpoint");
                  path := Some value)
          | "abstract" -> (
              match !abstract with
              | Some _ ->
                  raise
                    (Unsupported_input "duplicate unix address option abstract")
              | None ->
                  if value = "" then
                    raise (Unsupported_input "empty unix abstract endpoint");
                  abstract := Some value)
          | "guid" -> (
              match !guid with
              | Some previous when previous <> value ->
                  raise (Unsupported_input "conflicting unix address guid")
              | Some _ -> ()
              | None -> guid := Some value)
          | other ->
              raise
                (Unsupported_input ("unsupported unix address option " ^ other)))
        (split_options rest);
      match (!path, !abstract) with
      | Some _, Some _ ->
          raise (Unsupported_input "conflicting unix endpoint options")
      | Some path, None -> { endpoint = Filesystem path; guid = !guid }
      | None, Some name -> { endpoint = Abstract name; guid = !guid }
      | None, None ->
          raise (Unsupported_input "unix address missing path or abstract"))

let session_bus_address_string () =
  match Sys.getenv_opt "DBUS_SESSION_BUS_ADDRESS" with
  | Some value -> value
  | None ->
      let runtime =
        match Sys.getenv_opt "XDG_RUNTIME_DIR" with
        | Some value -> value
        | None -> "/run/user/" ^ string_of_int (Unix.getuid ())
      in
      "unix:path=" ^ Filename.concat runtime "bus"

let session_bus_address () = parse_bus_address (session_bus_address_string ())

let session_bus_sockaddr () =
  match session_bus_address () with
  | { endpoint = Filesystem path; _ } -> `Unix path
  | { endpoint = Abstract name; _ } -> `Unix ("\000" ^ name)

let session_bus_path () =
  match session_bus_address () with
  | { endpoint = Filesystem path; _ } -> path
  | { endpoint = Abstract _; _ } ->
      raise (Unsupported_input "unix:abstract is not a filesystem path")

let write flow s = Eio.Flow.write flow [ Cstruct.of_string s ]

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
  let add c =
    if Buffer.length buf >= max_auth_line_bytes then
      raise (Unsupported_input "D-Bus auth line exceeds 4KiB");
    Buffer.add_char buf c
  in
  let rec loop last_cr =
    let b = read_exact flow 1 in
    let c = Bytes.get b 0 in
    if last_cr && c = '\n' then Buffer.contents buf
    else (
      if last_cr then add '\r';
      if c = '\r' then loop true
      else (
        add c;
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

let make_method_call ?signature ?body ~serial ~destination ~path ~interface
    ~member () =
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
  serial : int;
  reply_serial : int option;
  path : string option;
  interface : string option;
  member : string option;
  error_name : string option;
  sender : string option;
  destination : string option;
  body_signature : string option;
  body : bytes;
}

let require off need bound what =
  if off < 0 || need < 0 || off > bound || bound - off < need then
    raise (Unsupported_input ("truncated D-Bus " ^ what))

let parse_dbus_string bytes off bound =
  require off 4 bound "string length";
  let len = get_u32 bytes off in
  if len < 0 || len > max_message_bytes then
    raise (Unsupported_input "D-Bus string exceeds 64KiB");
  require (off + 4) (len + 1) bound "string";
  let start = off + 4 in
  if Bytes.get bytes (start + len) <> '\000' then
    raise (Unsupported_input "D-Bus string missing NUL terminator");
  for i = 0 to len - 1 do
    if Bytes.get bytes (start + i) = '\000' then
      raise (Unsupported_input "D-Bus string contains interior NUL")
  done;
  (Bytes.sub_string bytes start len, start + len + 1)

let parse_signature bytes off bound =
  require off 1 bound "signature length";
  let len = Char.code (Bytes.get bytes off) in
  require (off + 1) (len + 1) bound "signature";
  if Bytes.get bytes (off + 1 + len) <> '\000' then
    raise (Unsupported_input "D-Bus signature missing NUL terminator");
  (Bytes.sub_string bytes (off + 1) len, off + 1 + len + 1)

let expected_header_signature = function
  | 1 -> "o"
  | 2 | 3 | 4 | 6 | 7 -> "s"
  | 5 | 9 -> "u"
  | 8 -> "g"
  | code ->
      raise
        (Unsupported_input
           (Printf.sprintf "invalid D-Bus header field code %d" code))

let parse_header_fields bytes len =
  let seen = Array.make 10 false in
  let rec loop off reply_serial path interface member error_name sender
      destination body_signature =
    if off >= len then
      ( reply_serial,
        path,
        interface,
        member,
        error_name,
        sender,
        destination,
        body_signature )
    else
      let off = align off 8 in
      if off >= len then
        ( reply_serial,
          path,
          interface,
          member,
          error_name,
          sender,
          destination,
          body_signature )
      else (
        require off 1 len "header field";
        let code = Char.code (Bytes.get bytes off) in
        let expected = expected_header_signature code in
        if seen.(code) then
          raise (Unsupported_input "duplicate D-Bus header field");
        seen.(code) <- true;
        let signature, value_off = parse_signature bytes (off + 1) len in
        if signature <> expected then
          raise (Unsupported_input "D-Bus header field signature mismatch");
        let value_off =
          match signature with
          | "s" | "o" | "u" -> align value_off 4
          | "g" -> value_off
          | other ->
              raise
                (Unsupported_input
                   ("unsupported D-Bus header signature " ^ other))
        in
        let ( next,
              reply_serial,
              path,
              interface,
              member,
              error_name,
              sender,
              destination,
              body_signature ) =
          match (code, signature) with
          | 1, "o" ->
              let value, next = parse_dbus_string bytes value_off len in
              ( next,
                reply_serial,
                Some value,
                interface,
                member,
                error_name,
                sender,
                destination,
                body_signature )
          | (2 | 3 | 4 | 6 | 7), "s" ->
              let value, next = parse_dbus_string bytes value_off len in
              let interface = if code = 2 then Some value else interface in
              let member = if code = 3 then Some value else member in
              let error_name = if code = 4 then Some value else error_name in
              let destination = if code = 6 then Some value else destination in
              let sender = if code = 7 then Some value else sender in
              ( next,
                reply_serial,
                path,
                interface,
                member,
                error_name,
                sender,
                destination,
                body_signature )
          | 5, "u" ->
              require value_off 4 len "uint32 header";
              ( value_off + 4,
                Some (get_u32 bytes value_off),
                path,
                interface,
                member,
                error_name,
                sender,
                destination,
                body_signature )
          | 9, "u" ->
              require value_off 4 len "unix fds header";
              ( value_off + 4,
                reply_serial,
                path,
                interface,
                member,
                error_name,
                sender,
                destination,
                body_signature )
          | 8, "g" ->
              let value, next = parse_signature bytes value_off len in
              ( next,
                reply_serial,
                path,
                interface,
                member,
                error_name,
                sender,
                destination,
                Some value )
          | _ ->
              raise (Unsupported_input "D-Bus header field signature mismatch")
        in
        loop next reply_serial path interface member error_name sender
          destination body_signature)
  in
  loop 0 None None None None None None None None

let parse_message_bytes bytes =
  if Bytes.length bytes < 16 then
    raise (Unsupported_input "truncated D-Bus header");
  if Bytes.get bytes 0 <> 'l' then
    raise (Unsupported_input "unsupported D-Bus endianness");
  let msg_type = Char.code (Bytes.get bytes 1) in
  let body_len = get_u32 bytes 4 in
  let serial = get_u32 bytes 8 in
  let fields_len = get_u32 bytes 12 in
  if fields_len > max_message_bytes || body_len > max_message_bytes then
    raise (Unsupported_input "D-Bus message exceeds 64KiB");
  let header_len = align (16 + fields_len) 8 in
  if header_len > max_message_bytes || header_len + body_len > max_message_bytes
  then raise (Unsupported_input "D-Bus message exceeds 64KiB");
  if Bytes.length bytes < header_len + body_len then
    raise (Unsupported_input "truncated D-Bus message");
  let fields = Bytes.sub bytes 16 fields_len in
  let body = Bytes.sub bytes header_len body_len in
  let ( reply_serial,
        path,
        interface,
        member,
        error_name,
        sender,
        destination,
        body_signature ) =
    parse_header_fields fields fields_len
  in
  {
    msg_type;
    serial;
    reply_serial;
    path;
    interface;
    member;
    error_name;
    sender;
    destination;
    body_signature;
    body;
  }

let read_message flow =
  let fixed = read_exact flow 16 in
  if Bytes.get fixed 0 <> 'l' then
    raise (Unsupported_input "unsupported D-Bus endianness");
  let body_len = get_u32 fixed 4 in
  let fields_len = get_u32 fixed 12 in
  if fields_len > max_message_bytes || body_len > max_message_bytes then
    raise (Unsupported_input "D-Bus message exceeds 64KiB");
  let header_len = align (16 + fields_len) 8 in
  if header_len > max_message_bytes || header_len + body_len > max_message_bytes
  then raise (Unsupported_input "D-Bus message exceeds 64KiB");
  let rest = read_exact flow (header_len - 16 + body_len) in
  let bytes = Bytes.create (header_len + body_len) in
  Bytes.blit fixed 0 bytes 0 16;
  Bytes.blit rest 0 bytes 16 (Bytes.length rest);
  parse_message_bytes bytes

let parse_string_body body =
  let value, next = parse_dbus_string body 0 (Bytes.length body) in
  if next <> Bytes.length body then
    raise (Unsupported_input "trailing D-Bus string body bytes");
  value

let parse_name_owner_changed_body body =
  let bound = Bytes.length body in
  let name, off = parse_dbus_string body 0 bound in
  let off = align off 4 in
  let old_owner, off = parse_dbus_string body off bound in
  let off = align off 4 in
  let new_owner, off = parse_dbus_string body off bound in
  if off <> bound then
    raise (Unsupported_input "trailing NameOwnerChanged body bytes");
  (name, old_owner, new_owner)

let parse_key_event ~owner msg =
  match (msg.msg_type, msg.sender, msg.path, msg.interface, msg.member) with
  | 4, Some sender, Some path, Some iface, Some "KeyEvent"
    when sender = owner && path = a11y_path && iface = a11y_interface ->
      (match msg.body_signature with
      | Some "buuuq" -> ()
      | _ -> raise (Unsupported_input "unsupported KeyEvent signature"));
      let body = msg.body in
      if Bytes.length body <> 18 then
        raise (Unsupported_input "unsupported KeyEvent body length");
      let released_u = get_u32 body 0 in
      if released_u <> 0 && released_u <> 1 then
        raise (Unsupported_input "noncanonical D-Bus boolean");
      let state = get_u32 body 4 in
      let keysym = get_u32 body 8 in
      let unichar = get_u32 body 12 in
      let keycode = get_u16 body 16 in
      Some { released = released_u <> 0; state; keysym; unichar; keycode }
  | _ -> None

type pin = {
  mutable a11y_owner : string option;
  mutable bus_unique : string option;
  mutable lost : bool;
}

let is_unique_name name = String.length name >= 2 && name.[0] = ':'

let sender_is_bus pin = function
  | Some s -> s = dbus_service || pin.bus_unique = Some s
  | None -> false

let reply_sender_ok pin expected msg =
  match expected with
  | `Bus -> sender_is_bus pin msg.sender
  | `Unique name -> msg.sender = Some name

let handle_name_owner_changed pin msg =
  if
    msg.msg_type = 4 && msg.path = Some dbus_path
    && msg.interface = Some dbus_interface
    && msg.member = Some "NameOwnerChanged"
    && sender_is_bus pin msg.sender
  then
    match msg.body_signature with
    | Some "sss" -> (
        let name, _old_owner, new_owner =
          parse_name_owner_changed_body msg.body
        in
        if name = a11y_service then
          match pin.a11y_owner with
          | None ->
              if new_owner = "" then pin.lost <- true
              else pin.a11y_owner <- Some new_owner
          | Some current -> if new_owner <> current then pin.lost <- true)
    | None | Some _ ->
        raise (Unsupported_input "unsupported NameOwnerChanged signature")

let hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_hex_guid s =
  String.length s = 32
  &&
  let rec loop i = i = 32 || (hex_digit s.[i] && loop (i + 1)) in
  loop 0

let accept_auth_ok_line line =
  match String.length line with
  | 35 when String.sub line 0 3 = "OK " && is_hex_guid (String.sub line 3 32) ->
      ()
  | _ -> raise (Unsupported_input "D-Bus auth response rejected")

let require_string_reply msg =
  match msg.body_signature with
  | Some "s" -> parse_string_body msg.body
  | _ -> raise (Unsupported_input "unexpected GetNameOwner signature")

let require_empty_reply msg =
  (match msg.body_signature with
  | None | Some "" -> ()
  | Some _ -> raise (Unsupported_input "unexpected WatchKeyboard signature"));
  if Bytes.length msg.body <> 0 then
    raise (Unsupported_input "unexpected WatchKeyboard body")

let authenticate flow =
  let uid = string_of_int (Unix.getuid ()) in
  let hex_uid =
    String.concat ""
      (List.init (String.length uid) (fun i ->
           Printf.sprintf "%02x" (Char.code uid.[i])))
  in
  write flow ("\000AUTH EXTERNAL " ^ hex_uid ^ "\r\n");
  let line = read_crlf_line flow in
  accept_auth_ok_line line;
  write flow "BEGIN\r\n"

let dbus_string_body s =
  let buf = Buffer.create (String.length s + 8) in
  add_dbus_string buf s;
  Buffer.contents buf

let call_method pin flow serial ~reply_from ~destination ~path ~interface
    ~member ?signature ?body () =
  write flow
    (make_method_call ?signature ?body ~serial ~destination ~path ~interface
       ~member ());
  let rec wait () =
    if pin.lost then raise Input_lost;
    let msg = read_message flow in
    handle_name_owner_changed pin msg;
    if pin.lost then raise Input_lost;
    match (msg.msg_type, msg.reply_serial) with
    | 2, Some reply when reply = serial ->
        if not (reply_sender_ok pin reply_from msg) then
          raise (Unsupported_input "D-Bus method reply sender mismatch");
        msg
    | 3, Some reply when reply = serial ->
        let error_from_expected =
          match reply_from with
          | `Bus -> false
          | `Unique name -> msg.sender = Some name
        in
        if not (sender_is_bus pin msg.sender || error_from_expected) then
          raise (Unsupported_input "D-Bus error sender mismatch");
        let name = Option.value ~default:"<unknown>" msg.error_name in
        failwith (member ^ " failed: " ^ name)
    | _ -> wait ()
  in
  wait ()

let add_match pin flow serial rule =
  ignore
    (call_method pin flow serial ~reply_from:`Bus ~destination:dbus_service
       ~path:dbus_path ~interface:dbus_interface ~member:"AddMatch"
       ~signature:"s" ~body:(dbus_string_body rule) ())

let combine_primary_and_cleanup primary cleanup =
  match primary with
  | Eio.Cancel.Cancelled _, _ ->
      (Eio.Exn.Multiple [ cleanup; primary ], snd primary)
  | _ -> Eio.Exn.combine primary cleanup

let reraise_after_close ~close flow (primary_exn, primary_bt) =
  match
    try
      close flow;
      None
    with close_exn -> Some (close_exn, Printexc.get_raw_backtrace ())
  with
  | None -> Printexc.raise_with_backtrace primary_exn primary_bt
  | Some cleanup ->
      let exn, bt =
        combine_primary_and_cleanup (primary_exn, primary_bt) cleanup
      in
      Printexc.raise_with_backtrace exn bt

let setup_watch ~close ~sw ~net =
  let flow = Eio.Net.connect ~sw net (session_bus_sockaddr ()) in
  match
    try
      let pin = { a11y_owner = None; bus_unique = None; lost = false } in
      authenticate flow;
      ignore
        (call_method pin flow 1 ~reply_from:`Bus ~destination:dbus_service
           ~path:dbus_path ~interface:dbus_interface ~member:"Hello" ());
      add_match pin flow 2
        ("type='signal',interface='" ^ a11y_interface ^ "',member='KeyEvent'");
      add_match pin flow 3
        ("type='signal',sender='" ^ dbus_service ^ "',path='" ^ dbus_path
       ^ "',interface='" ^ dbus_interface ^ "',member='NameOwnerChanged',arg0='"
       ^ a11y_service ^ "'");
      let bus_msg =
        call_method pin flow 4 ~reply_from:`Bus ~destination:dbus_service
          ~path:dbus_path ~interface:dbus_interface ~member:"GetNameOwner"
          ~signature:"s"
          ~body:(dbus_string_body dbus_service)
          ()
      in
      let bus_owner = require_string_reply bus_msg in
      if not (bus_owner = dbus_service || is_unique_name bus_owner) then
        raise (Unsupported_input "bus unique name expected");
      if is_unique_name bus_owner then pin.bus_unique <- Some bus_owner;
      let owner_msg =
        call_method pin flow 5 ~reply_from:`Bus ~destination:dbus_service
          ~path:dbus_path ~interface:dbus_interface ~member:"GetNameOwner"
          ~signature:"s"
          ~body:(dbus_string_body a11y_service)
          ()
      in
      let owner = require_string_reply owner_msg in
      if not (is_unique_name owner) then
        raise (Unsupported_input "a11y unique name expected");
      (match pin.a11y_owner with
      | Some current when current <> owner -> raise Input_lost
      | _ -> pin.a11y_owner <- Some owner);
      if pin.lost then raise Input_lost;
      require_empty_reply
        (call_method pin flow 6 ~reply_from:(`Unique owner) ~destination:owner
           ~path:a11y_path ~interface:a11y_interface ~member:"WatchKeyboard" ());
      if pin.lost then raise Input_lost;
      Ok (flow, owner, pin)
    with exn -> Error (exn, Printexc.get_raw_backtrace ())
  with
  | Ok conn -> conn
  | Error err -> reraise_after_close ~close flow err

let connect_and_watch_with ~close ~sw ~net =
  let flow, _owner, _pin = setup_watch ~close ~sw ~net in
  flow

let connect_and_watch ~sw ~net =
  connect_and_watch_with ~close:Eio.Flow.close ~sw ~net

let ordinary_io_failure exn =
  match exn with
  | Eio.Exn.Io _ | End_of_file -> true
  | Unix.Unix_error (err, _, _) -> (
      match err with
      | Unix.EPIPE | Unix.ECONNRESET | Unix.ECONNABORTED | Unix.ENOTCONN
      | Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.ETIMEDOUT | Unix.EIO | Unix.EBADF
      | Unix.ECONNREFUSED ->
          true
      | _ -> false)
  | _ -> false

let unwatch_write ~destination flow =
  try
    write flow
      (make_method_call ~serial:7 ~destination ~path:a11y_path
         ~interface:a11y_interface ~member:"UnwatchKeyboard" ())
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn when ordinary_io_failure exn -> ()
  | exn -> raise exn

let unwatch_and_close_with ~close flow =
  let write_result =
    try
      unwatch_write ~destination:a11y_service flow;
      Ok ()
    with exn -> Error (exn, Printexc.get_raw_backtrace ())
  in
  match write_result with
  | Ok () -> close flow
  | Error (exn, bt) -> reraise_after_close ~close flow (exn, bt)

let unwatch_and_close flow = unwatch_and_close_with ~close:Eio.Flow.close flow

let read_loop ~owner pin flow t =
  let rec loop () =
    if pin.lost then raise Input_lost;
    let msg = try read_message flow with End_of_file -> raise Input_lost in
    handle_name_owner_changed pin msg;
    if pin.lost then raise Input_lost;
    match parse_key_event ~owner msg with
    | Some event ->
        emit t event;
        loop ()
    | None -> loop ()
  in
  loop ()

let with_monitor_budgets ~budgets ~sw ~net ~clock ~process_mgr:_ f =
  let t, flow, owner, pin =
    match
      Eta.Timeout.timeout ~clock budgets.acquire (fun () ->
          let t = create () in
          let flow, owner, pin = setup_watch ~close:Eio.Flow.close ~sw ~net in
          Ok (t, flow, owner, pin))
    with
    | Ok value -> value
    | Error `Timeout -> raise Setup_timeout
  in
  let release () =
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close flow)
      (fun () ->
        ignore
          (Eta.Timeout.timeout ~clock budgets.unwatch (fun () ->
               unwatch_write ~destination:owner flow;
               Ok ())))
  in
  Fun.protect ~finally:release @@ fun () ->
  Eta.Concurrent.with_background ~sw
    (fun () -> Ok (read_loop ~owner pin flow t))
    (fun () -> f t)

let with_monitor ~sw ~net ~clock ~process_mgr f =
  with_monitor_budgets ~budgets:production_budgets ~sw ~net ~clock ~process_mgr
    f
