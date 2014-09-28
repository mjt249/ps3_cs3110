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
  (*If a is < b then true, else false*)
  val ( < ) : t -> t -> bool
  (*if a is = to b then true, else false*)
  val ( === ) : t -> t -> bool
			    
  exception Unrepresentable

  val int_of_nat: t -> int
  val nat_of_int: int -> t
end

module IntNat : NATN = struct
  type t = int
     exception Unrepresentable
   let int_of_nat (x : t) : int= 
     if x >= max_int then (raise Unrepresentable)
    else x 

  let nat_of_int (x : int) : t= 
     if x < 0 then (raise Unrepresentable)
    else x

  let zero : t = 0
  let one : t = 1
  let ( + ) (t1: t) (t2: t) : t =
    nat_of_int(int_of_nat(t1) + int_of_nat(t2))   
 
  let ( * ) (t1: t) (t2: t) : t = 
    nat_of_int(int_of_nat(t1) * int_of_nat(t2))   

  let ( < ) (t1: t) (t2: t) : bool = 
    int_of_nat(t1) < int_of_nat(t2)

  let ( === ) (t1: t) (t2: t) : bool =
    int_of_nat(t1) = int_of_nat(t2)
end




module ListNat : NATN = struct
(* The list [a1; ...; an] represents the
* natural number n. That is , the list lst represents
* length ( lst ). The empty list represents 0. The values of
* the list elements are irrelevant . *)
  type t = int list
  exception Unrepresentable
  let one : t = [1]
  let zero : t = []
  let int_of_nat (x : t) : int =
      List.length(x)

  let rec add_x_to_length (current :int) (lst: t) : t =
    if (current <= 0) then lst
    else 
      add_x_to_length (current - 1) (1 :: lst) 
      
  let nat_of_int (x : int) : t =
    if x < 0 then (raise Unrepresentable) else
      add_x_to_length x []
      
  let ( + ) (t1: t) (t2: t) : t =
    if ((List.length(t1) + List.length(t2)) >= max_int)
      then (raise Unrepresentable)
    else 
      add_x_to_length (List.length(t2)) t1

  let ( * ) (t1: t) (t2: t) : t =
    let product = (List.length(t1) * List.length(t2)) in
    if(product >= max_int)
      then (raise Unrepresentable)
    else 
      add_x_to_length (product - (List.length(t1))) t1

  let ( < ) (t1: t) (t2: t) : bool =
    (List.length(t1)) < (List.length(t2))

  let ( === ) (t1: t) (t2: t) : bool =
    (List.length(t1)) = (List.length(t2))

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


