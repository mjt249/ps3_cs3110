open Nats
open Assertions
open Printf
open IntNat



TEST_UNIT "IntNat" =
let x = 3 in
let y = 5 in
let a = nat_of_int x in
let b = nat_of_int y in
let newx = int_of_nat a in
let newy = int_of_nat b in
assert_true ((x = newx) && (y = newy))

(* assert_raises (Some (Failure "")) eval (make_fact_tree (-1)) *)





let () = Pa_ounit_lib.Runtime.summarize() 