open Generic UnitTest

val () =
  unitTests (title "Simple get and set")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 10
         val () = set 9 true s
         val () = set 0 true s
       in
         thatEq bool {expect = get 0 s, actual = true};
         thatEq bool {expect = get 1 s, actual = false};
         thatEq bool {expect = get 8 s, actual = false};
         thatEq bool {expect = get 9 s, actual = true}
       end)) (title "any()")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 10
       in
         thatEq bool {expect = any s, actual = false};
         set 1 true s;
         thatEq bool {expect = any s, actual = true}
       end)) (title "all() for bitarray that has remainder")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 10
       in
         thatEq bool {expect = all s, actual = false};
         not s;
         thatEq bool {expect = all s, actual = true}
       end)) (title "setRange() for range inside same word")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 18

         val () = setRange 8 16 true s
         val is = List.tabulate (18, fn i => get i s)
         val expected =
           List.tabulate (8, fn _ => false) @ List.tabulate (8, fn _ => true)
           @ [false, false]
         val () = thatEq (list bool) {expect = expected, actual = is}

         val () = setRange 8 16 false s
         val () = setRange 0 8 true s
         val is = List.tabulate (18, fn i => get i s)
         val expected =
           List.tabulate (8, fn _ => true) @ List.tabulate (8, fn _ => false)
           @ [false, false]
         val () = thatEq (list bool) {expect = expected, actual = is}
       in
         ()
       end)) (title "setRange() across words")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 32
         val () = setRange 4 28 true s
         val is = List.tabulate (32, fn i => get i s)
         val fourFalse = List.tabulate (4, fn _ => false)
         val expected = fourFalse @ List.tabulate (24, fn _ => true) @ fourFalse
         val () = thatEq (list bool) {expect = expected, actual = is}

         val () = setRange 4 28 false s
         val is = List.tabulate (32, fn i => get i s)
         val expected = List.tabulate (32, fn _ => false)
         val () = thatEq (list bool) {expect = expected, actual = is}
       in
         ()
       end)) (title "Shift right 1 bit")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 10
         val () = set 1 true s
         val () = set 9 true s
         val () = shr 1 s
         val expected = "0100000001"
       in
         thatEq string {expect = expected, actual = toString s}
       end)) (title "Shift left 1 bit")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 10
         val () = set 1 true s
         val () = set 9 true s
         val () = shl 1 s
         val expected = "0000000100"
       in
         thatEq string {expect = expected, actual = toString s}
       end)) (title "Shift right 10 bits")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 18
         val () = List.app (fn i => set i true s) [2, 4, 5, 8, 10, 12, 13, 16]
         val () = shr 10 s
         val expected = "000000000001001101"
       in
         thatEq string {expect = expected, actual = toString s}
       end)) (title "Shift left 10 bits")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 18
         val () = List.app (fn i => set i true s) [2, 4, 5, 8, 10, 12, 13, 16]
         val () = shl 10 s
         val expected = "001101000000000000"
       in
         thatEq string {expect = expected, actual = toString s}
       end)) (title "Cloning bit vector")
    (test (fn () =>
       let
         open Word8BitVector
         val s = create 10
         val () = set 8 true s
         val s' = clone s
         val () = set 6 true s
         val () = set 4 true s'
       in
         thatEq string {expect = "0101000000", actual = toString s};
         thatEq string {expect = "0100010000", actual = toString s'}
       end)) $
