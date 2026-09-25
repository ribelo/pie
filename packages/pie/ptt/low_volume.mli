(** Pure advisory detector for mono 16 kHz signed PCM16 little-endian audio.
    This module neither cancels capture nor decides capture eligibility. *)

type t

val create : min_rms:float -> min_peak:float -> unit -> t
(** Start a new capture. Thresholds use amplitudes normalized by 32768, as in
    [Audio.Pcm]. Thresholds are fixed for the lifetime of the detector. *)

val process_chunk : t -> bytes -> t * bool
(** Return the new state and whether to emit the one advisory warning for this
    capture. Windows contain exactly 1600 samples (100 ms), aligned to the first
    input sample, independently of chunk boundaries. An unmatched low byte is
    carried into the next call; empty input has no effect.

    Warn after 30 consecutive complete windows whose peak is nonzero and whose
    RMS and peak are both strictly below the configured thresholds. An exactly
    silent window or a window meeting either threshold resets that run. Partial
    windows never trigger a warning. Once warned, subsequent calls return false;
    create a fresh detector for the next capture.

    State is immutable and bounded: partial-window statistics and at most one
    byte are retained, never the supplied buffer. *)
