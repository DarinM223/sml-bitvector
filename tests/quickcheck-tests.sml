open Generic UnitTest

fun pad init l1 l2 =
  l1 @ List.tabulate (List.length l2 - List.length l1, fn _ => init)

fun testOp bitOp opp (xs, ys) =
  let
    open Word8BitArray
    val maxLen = Int.max (List.length xs, List.length ys)
    val (xs, ys) =
      if List.length xs < List.length ys then (pad false xs ys, ys)
      else if List.length xs > List.length ys then (xs, pad false ys xs)
      else (xs, ys)
    val xs' = create maxLen
    val ys' = create maxLen
    val is = List.tabulate (maxLen, fn i => i)
    val () = List.app (fn (i, b) => set i b xs') (ListPair.zip (is, xs))
    val () = List.app (fn (i, b) => set i b ys') (ListPair.zip (is, ys))
    val () = bitOp xs' ys'
    val result = List.map opp (ListPair.zip (xs, ys))
  in
    List.app (fn (i, expected) => that (get i xs' = expected))
      (ListPair.zip (is, result))
  end

val () =
  unitTests (title "Simple get and set quickcheck")
    (testAll (list bool) (fn l =>
       let
         open Word16BitArray
         val s = create (List.length l)
         val is = List.tabulate (List.length l, fn i => i)
         val zipped = ListPair.zip (is, l)
         val () = List.app (fn (i, b) => set i b s) zipped
       in
         List.app (fn (i, b) => that (get i s = b)) zipped
       end)) (title "And quickcheck")
    (testAll (sq (list bool)) (testOp Word8BitArray.andd (fn (a, b) =>
       a andalso b))) (title "Or quickcheck")
    (testAll (sq (list bool)) (testOp Word8BitArray.or (fn (a, b) =>
       a orelse b))) (title "Xor quickcheck")
    (testAll (sq (list bool)) (testOp Word8BitArray.xor (fn (a, b) =>
       (a orelse b) andalso (not (a andalso b))))) $
