type model = { threshold : float; smoothing : float }

let default_model () = { threshold = 0.5; smoothing = 0.3 }

let process_frame (pcm : bytes) : float =
  let rms = Audio.Pcm.pcm_rms pcm in
  min 1.0 (rms *. 10.0)

type detector = {
  model : model;
  mutable score : float;
  mutable triggered : bool;
}

let create_detector ?(model = default_model ()) () =
  { model; score = 0.0; triggered = false }

let process detector pcm =
  let raw = process_frame pcm in
  (* Exponential temporal smoothing: blend the new raw score with the previous
     score so repeated detections reinforce one another and isolated spikes are
     damped. Blending toward a constant threshold (the old behaviour) ignored
     history entirely. *)
  let smoothing = detector.model.smoothing in
  let new_score =
    (raw *. (1.0 -. smoothing)) +. (detector.score *. smoothing)
  in
  detector.score <- new_score;
  detector.triggered <- new_score >= detector.model.threshold;
  new_score
