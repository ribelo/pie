type state = { score : float; triggered : bool }

let initial = { score = 0.0; triggered = false }
let update state score = { score; triggered = score > 0.5 }
