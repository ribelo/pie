let run () =
  Printf.printf "Wakeword training (data collection stub)\n%!";
  let base_dir = Paths.openwakeword_data_dir in
  let training_dir = Filename.concat base_dir "training" in
  let model_dir = Filename.concat training_dir "default" in
  let positive_dir = Filename.concat model_dir "positive" in
  let negative_dir = Filename.concat model_dir "negative" in
  let silence_dir = Filename.concat model_dir "silence" in
  List.iter Fs.mkdir_p
    [
      base_dir; training_dir; model_dir; positive_dir; negative_dir; silence_dir;
    ];
  Printf.printf "Training directories:\n%!";
  Printf.printf "  positive: %s\n%!" positive_dir;
  Printf.printf "  negative: %s\n%!" negative_dir;
  Printf.printf "  silence:  %s\n%!" silence_dir;
  (* Create a synthetic model metadata file *)
  let meta_path = Filename.concat model_dir "model.json" in
  let meta =
    `Assoc
      [
        ("name", `String "default");
        ("version", `String "1.0");
        ("samples_positive", `Int 0);
        ("samples_negative", `Int 0);
        ("samples_silence", `Int 0);
        ("onnx_ready", `Bool false);
      ]
  in
  let oc = open_out_bin meta_path in
  output_string oc (Yojson.Safe.to_string meta);
  close_out oc;
  Printf.printf "Wrote model metadata to %s\n%!" meta_path;
  Printf.printf
    "\nTo train a real model, record clips into the directories above,\n%!";
  Printf.printf
    "then run 'pie wakeword-tune' to compute detection thresholds.\n%!"
