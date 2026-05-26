type model = {
  threshold : float;
  smoothing : float;
}

let default_model () = { threshold = 0.5; smoothing = 0.3 }

let process_frame model (pcm : bytes) : float =
  let rms = Audio.Pcm.pcm_rms pcm in
  let score = min 1.0 (rms *. 10.0) in
  score *. (1.0 -. model.smoothing) +. model.threshold *. model.smoothing

type detector = {
  model : model;
  mutable score : float;
  mutable triggered : bool;
}

let create_detector ?(model = default_model ()) () =
  { model; score = 0.0; triggered = false }

let process detector pcm =
  let new_score = process_frame detector.model pcm in
  detector.score <- new_score;
  detector.triggered <- new_score >= detector.model.threshold;
  new_score
