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
  (*could be longer that max_int which would be unrepresentable.*)
  let int_of_nat (x : t) : int =
    if List.length(x) < max_int then List.length(x) 
    else 
      (raise Unrepresentable)

  let rec add_int_to_length (current :int) (lst: t) : t =
    if (current <= 0) then lst
    else 
      add_x_to_length (current - 1) (1 :: lst) 


  (*this one is specifically for adding lists together. 
    lists might be longer than max_int, which would be impossible
    to add with add_x_to_length*)
  let rec add_list_to_lenth (current_list :t) (lst: t) : t =
    match current_list with
     [] -> lst
    |hd::tail -> add_list_to_length tail (1::lst)
      
  let nat_of_int (x : int) : t =
    if x < 0 then (raise Unrepresentable) else
      add_int_to_length x []
      
  (*uses add_list_to_length now*)    
  let ( + ) (t1: t) (t2: t) : t =
    add_list_to_length t2 t1

  let ( * ) (t1: t) (t2: t) : t =
    let rec product_helper (t1: t) (t2: t) (product: t) : t =
      match t1 with
       [] -> product
      |hd::tail -> product_helper tail t2 (add_list_to_length t2 product) in
    product_helper t1 t2 []

  let rec compare_length (t1: t) (t2: t) : int =
    match t1, t2 with
      [], [] -> 0
    | [], hd::tl -> 1 (*second list longer*)
    | hd::tl, [] -> -1 (*first list longer*)
    | hd1::tl1, hd2::tl2 -> compare_length tl1 tl2  

  let ( < ) (t1: t) (t2: t) : bool =
    (compare_length t1 t2) = 1

  let ( === ) (t1: t) (t2: t) : bool =
    (compare_length t1 t2) = 0

end



module NatConvertFn ( N : NATN ) = struct
let int_of_nat (n : N.t ): int = N.int_of_nat(n)
let nat_of_int (n : int ): N.t = N.nat_of_int(n)
end

(*We need to deal with what happens when M.int_of_aliensym returns something more than max_int*)
(*Idea for testing: create int_of_aliensym such that everything is shifted +1 int?*)
module AlienNatFn (M: AlienMapping): NATN = struct 
  type t = M.aliensym list
  let zero : t = [M.zero]
  let one : t = [M.one]
  let addition (acc : t) (el : M.aliensym) = el::acc
  let ( + ) (t1: t) (t2: t) :t =  List.fold_left addition t1 t2
  let rec multiply (multiplier_t2: int) (lst : t) : t =
      if (multiplier_t2 <= 0) then lst 
      else multiply (multiplier_t2 - 1) (List.fold_left addition lst t1)
  let ( * ) (t1: t) (t2: t) :t =  multiply (int_of_nat(t2)) t1
  let ( < ) (t1: t) (t2: t) :bool= (int_of_nat(t1) < int_of_nat(t2))
  let ( === ) (t1: t) (t2: t) :bool = (int_of_nat(t1) = int_of_nat(t2))
  let int_of_nat (t1: t) : int = List.fold_left ( + ) 0 (List.map M.int_of_aliensym t1)
  let nat_of_int = (*My idea is to make a list of ones...?*)

end 

