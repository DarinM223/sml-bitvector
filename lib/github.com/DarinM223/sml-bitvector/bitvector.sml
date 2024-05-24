functor BitVectorOp (include WORD val opp: word * word -> word) =
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

functor BitVectorFn
  (structure WordN: WORD val zero: WordN.word val one: WordN.word) :> BIT_VECTOR =
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
  fun clone ({length, bits}: t) =
    { length = length
    , bits = Array.tabulate (Array.length bits, fn i => Array.sub (bits, i))
    }
  fun length ({length, ...}: t) = length
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

  fun alignTo (value, align) =
    (value + align - 1) div align * align

  fun setRange i j value (v: t) =
    let
      val orEquals = fn (a, b) =>
        Array.update (#bits v, a, WordN.orb (Array.sub (#bits v, a), b))
      val andEqualsNot = fn (a, b) =>
        Array.update (#bits v, a, WordN.andb
          (Array.sub (#bits v, a), WordN.notb b))
      val set = if value then orEquals else andEqualsNot
    in
      if i = j then
        ()
      else if i div WordN.wordSize = j div WordN.wordSize then
        let
          val imask = WordN.<< (one, Word.fromInt (i mod WordN.wordSize))
          val jmask = WordN.<< (one, Word.fromInt (j mod WordN.wordSize))
          val mask = WordN.- (jmask, imask)
        in
          set (i div WordN.wordSize, mask)
        end
      else
        let
          val prefixMask = WordN.<<
            (WordN.notb zero, Word.fromInt (i mod WordN.wordSize))
          val postfixMask = WordN.-
            (WordN.<< (one, Word.fromInt (j mod WordN.wordSize)), one)
          val fillMask = if value then WordN.notb zero else zero
          val () = set (i div WordN.wordSize, prefixMask)
          val i = ref (alignTo (i, WordN.wordSize))
        in
          while (!i + WordN.wordSize <= j) do
            ( Array.update (#bits v, !i div WordN.wordSize, fillMask)
            ; i := !i + WordN.wordSize
            );
          if !i < j then set (!i div WordN.wordSize, postfixMask) else ()
        end
    end

  structure AndOp = BitVectorOp (open WordN val opp = WordN.andb)
  structure OrOp = BitVectorOp (open WordN val opp = WordN.orb)
  structure XorOp = BitVectorOp (open WordN val opp = WordN.xorb)
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


  local
    fun wordShl (0, _) = ()
      | wordShl (n, {bits, ...}: t) =
          let
            val slice = ArraySlice.slice (bits, 0, SOME (Array.length bits - n))
            val rest = ArraySlice.slice (bits, 0, SOME n)
          in
            ArraySlice.copy {src = slice, dst = bits, di = n};
            ArraySlice.modify (fn _ => zero) rest
          end
    fun wordShr (0, _) = ()
      | wordShr (n, {bits, ...}: t) =
          let
            val slice = ArraySlice.slice (bits, n, NONE)
            val rest = ArraySlice.slice (bits, Array.length bits - n, NONE)
          in
            ArraySlice.copy {src = slice, dst = bits, di = 0};
            ArraySlice.modify (fn _ => zero) rest
          end

    fun maskTrailingOnes n =
      WordN.>> (WordN.fromInt ~1, Word.fromInt (WordN.wordSize - n))
    fun maskLeadingOnes n =
      WordN.notb (maskTrailingOnes (WordN.wordSize - n))
  in
    fun shl n (v: t) =
      let
        val () = wordShl (n div WordN.wordSize, v)
        val bitDistance = n mod WordN.wordSize
        val mask = maskLeadingOnes bitDistance
        val rsh = Word.fromInt (WordN.wordSize - bitDistance)
        val bitDistance = Word.fromInt bitDistance
        val i = ref (Array.length (#bits v) - 1)
      in
        if bitDistance = 0w0 then
          ()
        else
          ( while (!i > 0) do
              let
                val w = WordN.<< (Array.sub (#bits v, !i), bitDistance)
                val x = WordN.>>
                  (WordN.andb (Array.sub (#bits v, !i - 1), mask), rsh)
                val w = WordN.orb (w, x)
              in
                Array.update (#bits v, !i, w);
                i := !i - 1
              end
          ; Array.update (#bits v, 0, WordN.<<
              (Array.sub (#bits v, 0), bitDistance))
          )
      end

    fun shr n (v: t) =
      let
        val () = wordShr (n div WordN.wordSize, v)
        val bitDistance = n mod WordN.wordSize
        val mask = maskTrailingOnes bitDistance
        val lsh = Word.fromInt (WordN.wordSize - bitDistance)
        val bitDistance = Word.fromInt bitDistance
        val i = ref 0
        val endIndex = Array.length (#bits v) - 1
      in
        if bitDistance = 0w0 then
          ()
        else
          ( while (!i < endIndex) do
              let
                val w = WordN.>> (Array.sub (#bits v, !i), bitDistance)
                val x = WordN.<<
                  (WordN.andb (Array.sub (#bits v, !i + 1), mask), lsh)
                val w = WordN.orb (w, x)
              in
                Array.update (#bits v, !i, w);
                i := !i + 1
              end
          ; Array.update (#bits v, endIndex, WordN.>>
              (Array.sub (#bits v, endIndex), bitDistance))
          )
      end
  end

  fun toString (t as {length, ...}: t) =
    let
      val result: bool list ref = ref []
      val i = ref 0
    in
      while (!i < length) do (result := get (!i) t :: !result; i := !i + 1);
      implode (map (fn true => #"1" | false => #"0") (!result))
    end
end

structure Word8BitVector =
  BitVectorFn
    (structure WordN = Word8
     val zero: Word8.word = 0w0
     val one: Word8.word = 0w1)

structure Word32BitVector =
  BitVectorFn
    (structure WordN = Word32
     val zero: Word32.word = 0w0
     val one: Word32.word = 0w1)
