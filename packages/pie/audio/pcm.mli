val default_auto_gain_target_rms : float
val default_auto_gain_max : float
val default_auto_gain_peak_limit : float
val min_gain_to_apply : float
val ptt_min_capture_rms_for_stt : float
val ptt_min_capture_peak_for_stt : float

val build_pcm_wav_header : data_length:int -> sample_rate:int -> bytes
val decode_s16le_samples : bytes -> int array
val pcm_rms : bytes -> float
val pcm_peak : bytes -> float

type normalization_config = {
  pcm_bytes : bytes;
  target_rms : float;
  max_gain : float;
  peak_limit : float;
  silence_rms_threshold : float;
  silence_peak_threshold : float;
}

val compute_normalization_gain : normalization_config -> float

type normalization_result = {
  normalized_bytes : bytes;
  gain : float;
}

val normalize_pcm_s16le_target_rms : normalization_config -> normalization_result
val normalize_pcm_for_stt :
  ?silence_rms_threshold:float ->
  ?silence_peak_threshold:float ->
  bytes ->
  normalization_result
