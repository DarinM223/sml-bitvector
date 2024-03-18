open Generic UnitTest

val () =
  unitTests (title "Simple get and set")
    (test (fn () =>
       let
         open Word8BitArray
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
         open Word8BitArray
         val s = create 10
       in
         thatEq bool {expect = any s, actual = false};
         set 1 true s;
         thatEq bool {expect = any s, actual = true}
       end)) (title "all() for bitarray that has remainder")
    (test (fn () =>
       let
         open Word8BitArray
         val s = create 10
       in
         thatEq bool {expect = all s, actual = false};
         not s;
         thatEq bool {expect = all s, actual = true}
       end)) (title "setRange() for range inside same word")
    (test (fn () =>
       let
         open Word8BitArray
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
         open Word8BitArray
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
       end)) $
