let default_auto_gain_target_rms = 0.12
let default_auto_gain_max = 40.0
let default_auto_gain_peak_limit = 0.95
let min_gain_to_apply = 1.05

let ptt_min_capture_rms_for_stt = 0.0015
let ptt_min_capture_peak_for_stt = 0.01

let build_pcm_wav_header ~(data_length : int) ~(sample_rate : int) : bytes =
  let header = Bytes.create 44 in
  let write_string offset value =
    String.iteri (fun i c -> Bytes.set_uint8 header (offset + i) (Char.code c)) value
  in
  write_string 0 "RIFF";
  Bytes.set_int32_le header 4 (Int32.of_int (36 + data_length));
  write_string 8 "WAVE";
  write_string 12 "fmt ";
  Bytes.set_int32_le header 16 (Int32.of_int 16);
  Bytes.set_uint16_le header 20 1;
  Bytes.set_uint16_le header 22 1;
  Bytes.set_int32_le header 24 (Int32.of_int sample_rate);
  Bytes.set_int32_le header 28 (Int32.of_int (sample_rate * 2));
  Bytes.set_uint16_le header 32 2;
  Bytes.set_uint16_le header 34 16;
  write_string 36 "data";
  Bytes.set_int32_le header 40 (Int32.of_int data_length);
  header

let decode_s16le_samples (pcm_bytes : bytes) : int array =
  let sample_count = Bytes.length pcm_bytes / 2 in
  Array.init sample_count (fun i ->
    let b0 = Bytes.get_uint8 pcm_bytes (i * 2) in
    let b1 = Bytes.get_uint8 pcm_bytes (i * 2 + 1) in
    let raw = (b1 lsl 8) lor b0 in
    if raw > 32767 then raw - 65536 else raw)

let pcm_rms (chunk : bytes) : float =
  let sample_count = Bytes.length chunk / 2 in
  if sample_count <= 0 then 0.0
  else begin
    let sum_squares = ref 0.0 in
    for i = 0 to sample_count - 1 do
      let b0 = Bytes.get_uint8 chunk (i * 2) in
      let b1 = Bytes.get_uint8 chunk (i * 2 + 1) in
      let raw = (b1 lsl 8) lor b0 in
      let sample = float_of_int (if raw > 32767 then raw - 65536 else raw) /. 32768.0 in
      sum_squares := !sum_squares +. (sample *. sample)
    done;
    sqrt (!sum_squares /. float_of_int sample_count)
  end

let pcm_peak (chunk : bytes) : float =
  let sample_count = Bytes.length chunk / 2 in
  if sample_count <= 0 then 0.0
  else begin
    let peak = ref 0.0 in
    for i = 0 to sample_count - 1 do
      let b0 = Bytes.get_uint8 chunk (i * 2) in
      let b1 = Bytes.get_uint8 chunk (i * 2 + 1) in
      let raw = (b1 lsl 8) lor b0 in
      let normalized =
        abs_float (float_of_int (if raw > 32767 then raw - 65536 else raw) /. 32768.0)
      in
      if normalized > !peak then peak := normalized
    done;
    !peak
  end

type normalization_config = {
  pcm_bytes : bytes;
  target_rms : float;
  max_gain : float;
  peak_limit : float;
  silence_rms_threshold : float;
  silence_peak_threshold : float;
}

let compute_normalization_gain (config : normalization_config) : float =
  let rms = pcm_rms config.pcm_bytes in
  let peak = pcm_peak config.pcm_bytes in
  if rms <= 0.0 || peak <= 0.0 then 1.0
  else if rms < config.silence_rms_threshold && peak < config.silence_peak_threshold then 1.0
  else begin
    let target_gain =
      min config.max_gain (max 1.0 (config.target_rms /. rms))
    in
    let peak_limited_gain =
      min config.max_gain (max 1.0 (config.peak_limit /. peak))
    in
    min target_gain peak_limited_gain
  end

type normalization_result = {
  normalized_bytes : bytes;
  gain : float;
}

let normalize_pcm_s16le_target_rms (config : normalization_config) : normalization_result =
  let gain = compute_normalization_gain config in
  if gain <= min_gain_to_apply then
    { normalized_bytes = Bytes.copy config.pcm_bytes; gain }
  else begin
    let len = Bytes.length config.pcm_bytes in
    let out = Bytes.create len in
    for i = 0 to (len / 2) - 1 do
      let b0 = Bytes.get_uint8 config.pcm_bytes (i * 2) in
      let b1 = Bytes.get_uint8 config.pcm_bytes (i * 2 + 1) in
      let raw = (b1 lsl 8) lor b0 in
      let sample = if raw > 32767 then raw - 65536 else raw in
      let boosted =
        max (-32768) (min 32767 (int_of_float (float_of_int sample *. gain)))
      in
      let boosted_raw = if boosted < 0 then boosted + 65536 else boosted in
      Bytes.set_uint8 out (i * 2) (boosted_raw land 0xff);
      Bytes.set_uint8 out (i * 2 + 1) ((boosted_raw lsr 8) land 0xff)
    done;
    if len mod 2 <> 0 then
      Bytes.set_uint8 out (len - 1) (Bytes.get_uint8 config.pcm_bytes (len - 1));
    { normalized_bytes = out; gain }
  end

let normalize_pcm_for_stt
    ?(silence_rms_threshold = ptt_min_capture_rms_for_stt)
    ?(silence_peak_threshold = ptt_min_capture_peak_for_stt)
    (pcm_bytes : bytes) : normalization_result =
  normalize_pcm_s16le_target_rms {
    pcm_bytes;
    target_rms = default_auto_gain_target_rms;
    max_gain = default_auto_gain_max;
    peak_limit = default_auto_gain_peak_limit;
    silence_rms_threshold;
    silence_peak_threshold;
  }
