type t = {
  min_rms : float;
  min_peak : float;
  samples : int;
  sum_squares : float;
  peak : int;
  pending : int option;
  low_windows : int;
  warned : bool;
}

let create ~min_rms ~min_peak () =
  {
    min_rms;
    min_peak;
    samples = 0;
    sum_squares = 0.;
    peak = 0;
    pending = None;
    low_windows = 0;
    warned = false;
  }

let process_chunk t chunk =
  if t.warned || Bytes.length chunk = 0 then (t, false)
  else
    let samples = ref t.samples in
    let sum_squares = ref t.sum_squares in
    let peak = ref t.peak in
    let pending = ref t.pending in
    let low_windows = ref t.low_windows in
    let warned = ref false in
    let add_sample low high =
      let raw = low lor (high lsl 8) in
      let sample = if raw > 32767 then raw - 65536 else raw in
      let value = float_of_int sample in
      incr samples;
      sum_squares := !sum_squares +. (value *. value);
      peak := max !peak (abs sample);
      if !samples = 1600 then (
        let rms = sqrt (!sum_squares /. 1600.) /. 32768. in
        let normalized_peak = float_of_int !peak /. 32768. in
        if !peak <> 0 && rms < t.min_rms && normalized_peak < t.min_peak then
          incr low_windows
        else low_windows := 0;
        if !low_windows = 30 then warned := true;
        samples := 0;
        sum_squares := 0.;
        peak := 0)
    in
    let i = ref 0 in
    (match !pending with
    | None -> ()
    | Some low ->
        add_sample low (Bytes.get_uint8 chunk 0);
        pending := None;
        i := 1);
    while !i + 1 < Bytes.length chunk && not !warned do
      add_sample (Bytes.get_uint8 chunk !i) (Bytes.get_uint8 chunk (!i + 1));
      i := !i + 2
    done;
    if !i < Bytes.length chunk && not !warned then
      pending := Some (Bytes.get_uint8 chunk !i);
    ( {
        t with
        samples = !samples;
        sum_squares = !sum_squares;
        peak = !peak;
        pending = !pending;
        low_windows = !low_windows;
        warned = !warned;
      },
      !warned )
