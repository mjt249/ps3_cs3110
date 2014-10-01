open Nats
open Assertions
open Printf
open ListNat



TEST_UNIT "IntNat_and_ListNat" =
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
assert_true (x = newx)
let x = one in
let newx = nat_of_int 1 in
assert_true (x = newx)
let x = -5 in
assert_raises (Some Unrepresentable) nat_of_int x
let x = nat_of_int 3 in
let y = nat_of_int 5 in
let rslt = ( + ) x y in
assert_true ((int_of_nat rslt)  = 8)
let x = nat_of_int 3 in
let y = nat_of_int 5 in
let rslt = ( * ) x y in
assert_true ((int_of_nat rslt)  = 15)
(* let x = nat_of_int 461168601 in
let y = nat_of_int 461168602 in
assert_raises (Some Unrepresentable) (( * ) x y) *)
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
let x = nat_of_int(max_int - 3) in
let y = nat_of_int(15) in
let rslt = ( + ) x y in
assert_true (rslt <> y)









let () = Pa_ounit_lib.Runtime.summarize() 