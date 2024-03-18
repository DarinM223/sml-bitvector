signature BIT_ARRAY =
sig
  type t

  val create: int -> t
  val set: int -> bool -> t -> unit
  val setRange: int -> int -> bool -> t -> unit
  val get: int -> t -> bool
  val andd: t -> t -> unit
  val or: t -> t -> unit
  val xor: t -> t -> unit
  val not: t -> unit
  val any: t -> bool
  val all: t -> bool
end
