functor BitArrayOp (include WORD val opp: word * word -> word) =
struct
  type t = {length: int, bits: word array}
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

functor BitArrayFn
  (structure WordN: WORD val zero: WordN.word val one: WordN.word) :> BIT_ARRAY =
struct
  type t = {length: int, bits: WordN.word array}

  fun create length : t =
    { length = length
    , bits =
        Array.tabulate
          ( length div WordN.wordSize
            + (if length mod WordN.wordSize = 0 then 0 else 1)
          , fn _ => zero
          )
    }
  fun set i b (v: t) =
    let
      val i' = i div WordN.wordSize
      val combine = fn (x, y) =>
        if b then WordN.orb (x, y) else WordN.andb (x, WordN.notb y)
      val mask = WordN.<< (one, Word.fromInt (i mod WordN.wordSize))
    in
      Array.update (#bits v, i', combine (Array.sub (#bits v, i'), mask))
    end
  fun get i (v: t) =
    let
      val i' = i div WordN.wordSize
      val mask = WordN.<< (one, Word.fromInt (i mod WordN.wordSize))
    in
      WordN.andb (Array.sub (#bits v, i'), mask) <> zero
    end

  structure AndOp = BitArrayOp (open WordN val opp = WordN.andb)
  structure OrOp = BitArrayOp (open WordN val opp = WordN.orb)
  structure XorOp = BitArrayOp (open WordN val opp = WordN.xorb)
  val andd = AndOp.run
  val or = OrOp.run
  val xor = XorOp.run

  exception Break
  fun all (v: t) : bool =
    let
      val remainder = #length v mod WordN.wordSize
      val endIndex = #length v div WordN.wordSize
      val i = ref 0
    in
      while !i < endIndex do
        ( if Array.sub (#bits v, !i) <> WordN.notb zero then raise Break else ()
        ; i := !i + 1
        );
      remainder <= 0
      orelse
      let
        val w = Array.sub (#bits v, endIndex)
      in
        w = WordN.orb (WordN.- (WordN.<< (one, Word.fromInt remainder), one), w)
      end
    end
    handle Break => false
  val any: t -> bool = not o Array.all (fn w => w = zero) o #bits

  val not: t -> unit = Array.modify WordN.notb o #bits
end

structure Word8BitArray =
  BitArrayFn
    (structure WordN = Word8
     val zero: Word8.word = 0w0
     val one: Word8.word = 0w1)

structure Word16BitArray =
  BitArrayFn
    (structure WordN = Word16
     val zero: Word16.word = 0w0
     val one: Word16.word = 0w1)

structure Word32BitArray =
  BitArrayFn
    (structure WordN = Word32
     val zero: Word32.word = 0w0
     val one: Word32.word = 0w1)
