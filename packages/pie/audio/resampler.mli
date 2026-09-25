type error = [ `Resample_error of string ]
type t

val create : from_rate:int -> to_rate:int -> (t, error) result
(** Fails when either rate is not positive, or when the rates cannot be combined
    without overflowing representable interpolation arithmetic. *)

val push : t -> bytes -> (bytes, error) result
(** Convert samples whose lookahead is already present. Empty input is allowed.
    An incomplete trailing byte is held until the next push or [finish]. *)

val finish : t -> (bytes, error) result
(** Emit the held tail once, clamping the final next sample to the last source
    sample. A leftover odd byte is a format error. Repeated finish after success
    yields empty output; repeated finish after error returns that error. *)

val shift_count : t -> int
(** Retained-sample shifts performed. Advance is O(retained input), not
    O(ratio). *)
