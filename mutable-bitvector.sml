functor MutableBitVectorOp (val opp: Word8.word * Word8.word -> Word8.word) =
struct
  type t = {length: int, bits: Word8.word array}
  local
    fun run_ v1 v2 =
      Array.appi (fn (i, w) => Array.update (v1, i, opp (w, Array.sub (v2, i))))
        v1
  in
    fun run (v1: t) (v2: t) =
      if #length v1 = #length v2 then run_ (#bits v1) (#bits v2)
      else raise Fail "Invalid lengths"
  end
end

structure MutableBitVector :> MUTABLE_BITVECTOR =
struct
  type t = {length: int, bits: Word8.word array}

  fun create length : t =
    { length = length
    , bits =
        Array.tabulate
          ( length div Word8.wordSize
            + (if length mod Word8.wordSize = 0 then 0 else 1)
          , fn _ => 0w0
          )
    }
  fun set i b (v: t) =
    let
      val i' = i div Word8.wordSize
      val combine = fn (x, y) =>
        if b then Word8.orb (x, y) else Word8.andb (x, Word8.notb y)
      val setbit = Word8.<< (0w1, Word.fromInt (i mod Word8.wordSize))
    in
      Array.update (#bits v, i', combine (Array.sub (#bits v, i'), setbit))
    end
  fun get i v = raise Fail ""

  structure AndOp = MutableBitVectorOp(val opp = Word8.andb)
  structure OrOp = MutableBitVectorOp(val opp = Word8.orb)
  structure XorOp = MutableBitVectorOp(val opp = Word8.xorb)
  val andd = AndOp.run
  val or = OrOp.run
  val xor = XorOp.run

  exception Break
  fun all (v: t) : bool =
    let
      val remainder = #length v mod Word8.wordSize
      val endIndex = #length v div Word8.wordSize
      val i = ref 0
    in
      while !i < endIndex do
        ( if Array.sub (#bits v, !i) <> Word8.notb 0w0 then raise Break else ()
        ; i := !i + 1
        );
      remainder <= 0
      orelse
      Array.sub (#bits v, endIndex)
      = Word8.<< (0w1, Word.fromInt remainder) - 0w1
    end
    handle Break => false
  val any: t -> bool = not o Array.all (fn w => w = 0w0) o #bits

  val not: t -> unit = Array.modify Word8.notb o #bits
end
