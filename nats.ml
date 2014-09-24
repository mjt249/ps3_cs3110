(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
  type t
(*zero has the identity property that zero + t = t and zero * t = zero*)
  val zero : t
(*one has the identity property that 1 * t = 1 unless t is 0*)
  val one : t
(*(+) takes two ts and combines their ints 
* (+) is associative: (a + b) + c = a + (b + c)
* (+) is commutative: a + b = b + a
* Identity is 0 -> 0 + t = t
*)
  val ( + ) : t -> t -> t
(*( * )takes two ts and combines multiplies their ints 
* ( * ) is associative: (a * b) * c = a * (b * c)
* ( * ) is commutative: a * b = b * a
* Identity is 1 -> 1 * t = t
* multiplication is distributive over addition:
* a * (b + c) = (a * b) + (a * c)
*)
  val ( * ) : t -> t -> t 
  val ( < ) : t -> t -> bool
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  val int_of_nat: t -> int
  val nat_of_int: int -> t
end

module IntNat : NATN = struct
  type t = int
  
  let zero = 0
  let one = 1
  let ( + ) (t1: t) (t2: t) : t =
    nat_of_int(int_of_nat(t1) + int_of_nat(t2))   

  let ( * ) (t1: t) (t2: t) : t = 
    nat_of_int(int_of_nat(t1) * int_of_nat(t2))   

  let ( < ) (t1: t) (t2: t) : bool = 
    int_of_nat(t1) < int_of_nat(t2)

  let ( === ) (t1: t) (t2: t) : bool =
    int_of_nat(t1) = int_of_nat(t2)



  exception Unrepresentable

  let int_of_nat x= 
    if x <= 0 then (raise Unrepresentable)
    else x

  let nat_of_int x= 
    if x >= max_int then (raise Unrepresentable)
    else x 


end





module type AlienMapping = sig
  type aliensym

  val int_of_aliensym: aliensym -> int
  val one: aliensym
  val zero: aliensym
end

type sign = Positive | Negative
let sign_int (n:int) : sign = 
  if n >= 0 then Positive else Negative
let sum_overflows (i1:int) (i2:int) : bool = 
  sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
(* END: DO NOT CHANGE THIS CODE *)

(* Add your solution here for IntNat, ListNat, NatConvertFn, 
   and AlienNatFn, being careful to use the declarations and
   types specified in the problem set. *)


