type detector = {
  active : bool;
  consecutive_zero_chunks : int;
  warned_this_hold : bool;
  dead_this_hold : bool;
}

let warning_threshold = 4

let initial () =
  {
    active = false;
    consecutive_zero_chunks = 0;
    warned_this_hold = false;
    dead_this_hold = false;
  }

let sync detector is_active =
  if not is_active then initial ()
  else if not detector.active then
    {
      active = true;
      consecutive_zero_chunks = 0;
      warned_this_hold = false;
      dead_this_hold = false;
    }
  else detector

let is_flatline (pcm : bytes) : bool =
  let len = Bytes.length pcm in
  if len = 0 then false
  else
    let rec check i =
      if i >= len then true
      else if Bytes.get_uint8 pcm i <> 0 then false
      else check (i + 1)
    in
    check 0

type result = {
  detector : detector;
  warn : bool;
  dead : bool;
  has_input : bool;
}

let process_chunk detector chunk =
  if not detector.active then
    { detector; warn = false; dead = false; has_input = false }
  else if detector.dead_this_hold then
    { detector; warn = false; dead = true; has_input = false }
  else
    let is_flat = is_flatline chunk in
    let has_input = Bytes.length chunk > 0 && not is_flat in
    let next_consecutive =
      if is_flat then detector.consecutive_zero_chunks + 1 else 0
    in
    if next_consecutive < warning_threshold then
      {
        detector = { detector with consecutive_zero_chunks = next_consecutive };
        warn = false;
        dead = false;
        has_input;
      }
    else
      {
        detector =
          {
            active = true;
            consecutive_zero_chunks = next_consecutive;
            warned_this_hold = true;
            dead_this_hold = true;
          };
        warn = true;
        dead = true;
        has_input = false;
      }
