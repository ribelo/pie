type error = [ `Resample_error of string ]

type t = {
  from_rate : int;
  to_rate : int;
  equal_rate : bool;
  rest : int Queue.t;
  mutable prev : int option;
  mutable next : int option;
  mutable rem : int;
  mutable q : int;
  mutable n : int;
  mutable j : int;
  mutable odd : char option;
  mutable finished : bool;
  mutable finish_error : error option;
  mutable shift_count : int;
}

let rate_error = `Resample_error "sample rates must be positive"

let rate_unrepresentable =
  `Resample_error "sample rates exceed representable interpolation bounds"

let format_error = `Resample_error "PCM s16le stream ended on an odd byte"
let finished_error = `Resample_error "resampler already finished"

let overflow_error =
  `Resample_error "resampling arithmetic exceeds representable bounds"

let checked_add a b =
  if b > 0 && a > max_int - b then Error overflow_error
  else if b < 0 && a < min_int - b then Error overflow_error
  else Ok (a + b)

let rec checked_mul a b =
  if a = 0 || b = 0 then Ok 0
  else if a > 0 && b > 0 then
    if a > max_int / b then Error overflow_error else Ok (a * b)
  else if a < 0 && b < 0 then
    if a = min_int || b = min_int then Error overflow_error
    else checked_mul (-a) (-b)
  else
    let pos = max a b in
    let neg = min a b in
    if neg = min_int then Error overflow_error
    else if pos > 0 && -neg > max_int / pos then Error overflow_error
    else Ok (a * b)

let create ~from_rate ~to_rate =
  if from_rate <= 0 || to_rate <= 0 then Error rate_error
  else
    match checked_mul 32768 (max from_rate to_rate) with
    | Error _ -> Error rate_unrepresentable
    | Ok _ -> (
        match checked_add from_rate to_rate with
        | Error _ -> Error rate_unrepresentable
        | Ok _ ->
            Ok
              {
                from_rate;
                to_rate;
                equal_rate = from_rate = to_rate;
                rest = Queue.create ();
                prev = None;
                next = None;
                rem = 0;
                q = 0;
                n = 0;
                j = 0;
                odd = None;
                finished = false;
                finish_error = None;
                shift_count = 0;
              })

let clamp_int16 value =
  if value < -32768 then -32768 else if value > 32767 then 32767 else value

let interpolate ~to_rate ~rem a b =
  match checked_add to_rate (-rem) with
  | Error _ as error -> error
  | Ok to_minus_rem -> (
      match checked_mul a to_minus_rem with
      | Error _ as error -> error
      | Ok left -> (
          match checked_mul b rem with
          | Error _ as error -> error
          | Ok right -> (
              match checked_add left right with
              | Error _ as error -> error
              | Ok num -> Ok (clamp_int16 (num / to_rate)))))

let add_sample buf value =
  let v = clamp_int16 value land 0xffff in
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff))

let output_allowed t =
  if t.n <= 0 then Ok false
  else if t.j = 0 then Ok true
  else
    match checked_add t.j 1 with
    | Error _ as error -> error
    | Ok next_j -> (
        match checked_mul next_j t.from_rate with
        | Error _ as error -> error
        | Ok lhs -> (
            match checked_mul t.n t.to_rate with
            | Error _ as error -> error
            | Ok rhs -> Ok (lhs <= rhs)))

let shift t =
  t.shift_count <- t.shift_count + 1;
  t.prev <- t.next;
  t.next <- (if Queue.is_empty t.rest then None else Some (Queue.take t.rest))

let has_retained t =
  t.prev <> None || t.next <> None || not (Queue.is_empty t.rest)

let advance t dq =
  match checked_add t.q dq with
  | Error _ as error -> error
  | Ok q' ->
      let rec drop k =
        if k <= 0 || not (has_retained t) then ()
        else (
          shift t;
          drop (k - 1))
      in
      drop dq;
      t.q <- q';
      Ok ()

let shift_count t = t.shift_count

let emit_available t ~eos buf =
  let rec loop () =
    match output_allowed t with
    | Error _ as error -> error
    | Ok false -> Ok ()
    | Ok true -> (
        match t.prev with
        | None -> Ok ()
        | Some a -> (
            let need_next = t.rem <> 0 in
            let b =
              if not need_next then Some a
              else
                match t.next with
                | Some next -> Some next
                | None -> if eos then Some a else None
            in
            match b with
            | None -> Ok ()
            | Some b -> (
                match interpolate ~to_rate:t.to_rate ~rem:t.rem a b with
                | Error _ as error -> error
                | Ok sample -> (
                    add_sample buf sample;
                    match checked_add t.rem t.from_rate with
                    | Error _ as error -> error
                    | Ok sum -> (
                        let dq = sum / t.to_rate in
                        t.rem <- sum mod t.to_rate;
                        match checked_add t.j 1 with
                        | Error _ as error -> error
                        | Ok j' -> (
                            t.j <- j';
                            match advance t dq with
                            | Error _ as error -> error
                            | Ok () -> loop ()))))))
  in
  loop ()

let take_even_bytes t bytes =
  let len = Bytes.length bytes in
  let start_buf, start_off =
    match t.odd with
    | None -> (bytes, 0)
    | Some carried ->
        let combined = Bytes.create (len + 1) in
        Bytes.set combined 0 carried;
        Bytes.blit bytes 0 combined 1 len;
        t.odd <- None;
        (combined, 0)
  in
  let total = Bytes.length start_buf - start_off in
  let even = total land lnot 1 in
  if total > even then t.odd <- Some (Bytes.get start_buf (start_off + even));
  (start_buf, start_off, even)

let place t idx sample =
  if idx < t.q then ()
  else if t.prev = None then t.prev <- Some sample
  else if t.next = None then t.next <- Some sample
  else Queue.add sample t.rest

let projected_even t bytes =
  let extra = match t.odd with None -> 0 | Some _ -> 1 in
  (Bytes.length bytes + extra) land lnot 1

let closed_error t =
  match t.finish_error with
  | Some error -> Error error
  | None -> Error finished_error

let fail t error =
  t.finished <- true;
  t.finish_error <- Some error;
  Error error

let push t bytes =
  if t.finished then closed_error t
  else if Bytes.length bytes = 0 && t.odd = None then Ok Bytes.empty
  else
    let even = projected_even t bytes in
    if even = 0 then (
      ignore (take_even_bytes t bytes);
      Ok Bytes.empty)
    else
      match checked_add t.n (even / 2) with
      | Error error -> fail t error
      | Ok n' -> (
          let src, off, even' = take_even_bytes t bytes in
          if t.equal_rate then (
            let out = Bytes.sub src off even' in
            t.n <- n';
            t.j <- n';
            t.q <- n';
            t.prev <- None;
            t.next <- None;
            Queue.clear t.rest;
            Ok out)
          else
            let n0 = t.n in
            let samples = even' / 2 in
            for i = 0 to samples - 1 do
              place t (n0 + i) (Bytes.get_int16_le src (off + (i * 2)))
            done;
            t.n <- n';
            let buf = Buffer.create even' in
            match emit_available t ~eos:false buf with
            | Error error -> fail t error
            | Ok () -> Ok (Buffer.to_bytes buf))

let finish t =
  match t.finish_error with
  | Some error -> Error error
  | None when t.finished -> Ok Bytes.empty
  | None -> (
      t.finished <- true;
      if t.odd <> None then (
        t.finish_error <- Some format_error;
        Error format_error)
      else if t.n = 0 then Ok Bytes.empty
      else if t.equal_rate then Ok Bytes.empty
      else
        let buf = Buffer.create 16 in
        match emit_available t ~eos:true buf with
        | Error error ->
            t.finish_error <- Some error;
            Error error
        | Ok () -> Ok (Buffer.to_bytes buf))
