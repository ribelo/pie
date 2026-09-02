let run () =
  Printf.printf "Wakeword tuning (synthetic calibration)\n%!";
  (* Simulate scoring positive/negative/silence clips to compute threshold *)
  let positive_scores = [ 0.82; 0.91; 0.76; 0.88; 0.95 ] in
  let negative_scores = [ 0.12; 0.08; 0.15; 0.05; 0.22 ] in
  let silence_scores = [ 0.01; 0.00; 0.02; 0.01; 0.00 ] in
  let avg_positive =
    List.fold_left ( +. ) 0.0 positive_scores
    /. float_of_int (List.length positive_scores)
  in
  let avg_negative =
    List.fold_left ( +. ) 0.0 negative_scores
    /. float_of_int (List.length negative_scores)
  in
  let avg_silence =
    List.fold_left ( +. ) 0.0 silence_scores
    /. float_of_int (List.length silence_scores)
  in
  let threshold = (avg_positive +. avg_negative) /. 2.0 in
  Printf.printf "Positive avg: %.3f\n%!" avg_positive;
  Printf.printf "Negative avg: %.3f\n%!" avg_negative;
  Printf.printf "Silence avg:  %.3f\n%!" avg_silence;
  Printf.printf "Recommended threshold: %.3f\n%!" threshold;
  let tuning_path =
    Filename.concat Paths.wakeword_config_dir "detection-tuning.json"
  in
  let json =
    `Assoc
      [
        ("threshold", `Float threshold);
        ("smoothing", `Float 0.3);
        ("positive_avg", `Float avg_positive);
        ("negative_avg", `Float avg_negative);
        ("silence_avg", `Float avg_silence);
      ]
  in
  Fs.mkdir_p Paths.wakeword_config_dir;
  let oc = open_out_bin tuning_path in
  output_string oc (Yojson.Safe.to_string json);
  close_out oc;
  Printf.printf "Wrote tuning config to %s\n%!" tuning_path
