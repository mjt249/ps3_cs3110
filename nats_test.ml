open Nats
open Assertions
open Printf
open IntNat
(*open AlienConvert
open ListNat*)
(*we used the same test file and opened whatever we needed to test.
  there were some tests that were specifically for a module.*)

TEST_UNIT "IntNat_and_ListNat_and_AlienConvert" =
let x = 3 in
let y = 5 in
let a = nat_of_int x in
let b = nat_of_int y in
let newx = int_of_nat a in
let newy = int_of_nat b in
assert_true ((x = newx) && (y = newy))
let x = zero in
let newx = int_of_nat x in
assert_true (0 = newx)
let x = one in
let newx = int_of_nat x in
assert_true (1 = newx)
let x = zero in
let newx = nat_of_int 0 in
let equ = (===) x newx in
assert_true (equ)
let x = one in
let newx = nat_of_int 1 in
let equ = (===) x newx in
assert_true (equ)
let x = nat_of_int 3 in
let y = nat_of_int 5 in
let rslt = ( + ) x y in
assert_true ((int_of_nat rslt)  = 8)
let x = nat_of_int 3 in
let y = nat_of_int 5 in
let rslt = ( * ) x y in
assert_true ((int_of_nat rslt)  = 15)
let x = nat_of_int 3 in
let y = nat_of_int 5 in
let rslt = ( === ) x y in
assert_false (rslt)
let x = nat_of_int 3 in
let y = nat_of_int 3 in
let rslt = ( === ) x y in
assert_true (rslt)
let x = nat_of_int 3 in
let y = nat_of_int 3 in
let rslt = ( < ) x y in
assert_false (rslt)
let x = nat_of_int 3 in
let y = nat_of_int 5 in
let rslt = ( < ) x y in
assert_true (rslt)
let x = nat_of_int 5 in
let y = nat_of_int 3 in
let rslt = ( < ) x y in
assert_false (rslt)
let x = -5 in
assert_raises (Some Unrepresentable) nat_of_int x
(*only for IntNat*)
(*let x = nat_of_int(max_int - 3) in
let y = nat_of_int(15) in
assert_raises (Some Unrepresentable) (( + ) x) y
let x = nat_of_int 461168601 in
let y = nat_of_int 461168602 in
assert_raises (Some Unrepresentable) (( * ) x) y *)


let () = Pa_ounit_lib.Runtime.summarize() 