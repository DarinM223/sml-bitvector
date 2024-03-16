signature BIT_VECTOR =
sig
  type t

  val create: int -> t
  val set: int -> bool -> t
  val get: int -> t -> bool
  val andd: t -> t -> t
  val or: t -> t -> t
  val xor: t -> t -> t
  val not: t -> t
  val any: t -> bool
  val all: t -> bool
end
