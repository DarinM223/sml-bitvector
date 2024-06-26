signature BIT_VECTOR =
sig
  type t

  (* Creates a bit vector with the given length *)
  val create: int -> t
  (* Returns a copy of the bit vector *)
  val clone: t -> t
  val length: t -> int
  val set: int -> bool -> t -> unit
  val setRange: int -> int -> bool -> t -> unit
  val get: int -> t -> bool
  val andd: t -> t -> unit
  val or: t -> t -> unit
  val xor: t -> t -> unit
  val not: t -> unit
  val any: t -> bool
  val all: t -> bool
  val shr: int -> t -> unit
  val shl: int -> t -> unit
  (* Returns string of bit vector indexed from right to left starting at 0 *)
  val toString: t -> string
end
