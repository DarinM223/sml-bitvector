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
       end)) $
