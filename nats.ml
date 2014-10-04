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
(*Raises Unrepresentable if NATN argument passed is >= to max_int
*)
module IntNat : NATN = struct
    type t = int
         exception Unrepresentable
    let int_of_nat (x : t) : int= 
        x

    let nat_of_int (x : int) : t= 
        if x < 0 then (raise Unrepresentable)
        else x

     let zero : t = 0
     let one : t = 1
  

  (*neither of the two arguments have experienced overflow themselves.*)
  (*raise unrepresentable if result is larger than max_int*)
    let ( + ) (t1: t) (t2: t) : t =

        let int_t1 = int_of_nat t1 in
        let int_t2 = int_of_nat t2 in
            if (sum_overflows int_t1 int_t2) then (raise Unrepresentable)
            else
                nat_of_int(int_t1 + int_t2)


 
  (*neither of the two arguments have experienced overflow thenselves.*)
  (*raise unrepresentable if result is larger than max_int*)

    let ( * ) (t1: t) (t2: t) : t = 
        if (t1 = zero || t2 = zero) then zero
        else
            let rec multiply_helper (fst: t) (snd: t) (prod: t) : t = 
                if (fst = zero) then prod
                else
                    multiply_helper (nat_of_int((int_of_nat fst) -1)) snd (snd + prod) in
                        multiply_helper t1 t2 zero


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
        let rec int_of_nat_helper (lst: t) (counter: int) : int =
            if (counter < 0) then (raise Unrepresentable)
            else  
                match lst with
                 [] -> counter
                |hd::tail -> int_of_nat_helper tail (Pervasives.(+) counter 1) in
                      int_of_nat_helper x 0

    let rec add_int_to_length (current :int) (lst: t) : t =
        if (current <= 0) then lst
        else 
            add_int_to_length (Pervasives.(-) current 1) (1 :: lst) 


  (*this one is specifically for adding lists together. 
    lists might be longer than max_int, which would be impossible
    to add with add_x_to_length*)
    let rec add_list_to_length (current_list :t) (lst: t) : t =
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
        let rec product_helper (first: t) (second: t) (product: t) : t =
            match first with
             [] -> product
            |hd::tail -> product_helper tail second (add_list_to_length t2 product) in
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





module M : AlienMapping = struct
    type aliensym = float
    let int_of_aliensym (a : aliensym) : int = int_of_float a
    let one : aliensym = 1.0
    let zero : aliensym = 0.0
end







(*We need to deal with what happens when M.int_of_aliensym returns something more than max_int*)
(*Idea for testing: create int_of_aliensym such that everything is shifted +1 int? *)
module AlienNatFn (M: AlienMapping): NATN = struct
    type t = M.aliensym list
    exception Unrepresentable
    let zero : t = [M.zero]
    let one : t = [M.one]

    let add_helper (acc : t) (el : M.aliensym) = el::acc
  
    let ( + ) (t1: t) (t2: t) :t =  List.fold_left add_helper t1 t2

    let ( * ) (t1: t) (t2: t) :t = 
    (*takes a symbol, and appends the list symbol-times*)
    let sym_list_product (lst: t) (sym: M.aliensym) : t =
        let sym_int = M.int_of_aliensym(sym) in
        let rec sym_list_product_helper (counter: int) (acc: t) :t =
            if counter = 0 then acc
            else
               sym_list_product_helper (counter-1) (acc + lst) in
                   sym_list_product_helper sym_int [] in

    let rec prod_helper (first: t) (second: t) (acc:t) =
        match second with
         [] -> acc
        | hd::tl -> prod_helper first tl (acc + (sym_list_product first hd)) in
              prod_helper t1 t2 []

    (*requires, non empty list*)
    let rec all_zeros (lst: int list) (acc: bool) =
        match lst with
         [] -> acc
        |hd::tl -> if (hd = 0) then all_zeros tl true
                   else false

  (*takes two int lists, sorts differences of the elements according to sign 
  until at least one of the lists are empty.*)
    let rec compare_int_list (lst1: int list) (lst2: int list) (pos_dif: int list) (neg_dif: int list) : int =
        match lst1, lst2 with
         [], [] -> 
      (*done comparing the lists.*)
            (match pos_dif, neg_dif with
              [], [] -> 0
             | lst, [] -> if (all_zeros lst false) then 0 else 1(*more postives, first list larger*)
             | [], lst -> if (all_zeros lst false) then 0 else -1 (*more negatives, second list larger*)
             | pos, neg -> compare_int_list pos neg [] [])
        | hd::tl, [] -> compare_int_list tl [] (hd::pos_dif) neg_dif
        | [], hd::tl -> compare_int_list [] tl pos_dif (hd::neg_dif)
        | hd1::tl1, hd2::tl2 ->  
            let dif = hd1 - hd2 in
                if (dif < 0) then compare_int_list tl1 tl2 pos_dif ((-dif)::neg_dif)
                else if (dif = 0) then compare_int_list tl1 tl2 pos_dif neg_dif
                else compare_int_list tl1 tl2 (dif::pos_dif) neg_dif

  (*takes two alien sym lists and finds the differences, making the lists
  suitable for function compare_int_list*)
    let rec compare_alien_sym_list (t1:t) (t2:t) (pos_dif: int list) (neg_dif: int list): int =
        let t1_int_list = List.rev_map M.int_of_aliensym t1 in
        let t2_int_list = List.rev_map M.int_of_aliensym t2 in
        compare_int_list t1_int_list t2_int_list [] []

    let ( < ) (t1: t) (t2: t) :bool=
        (compare_alien_sym_list t1 t2 [] []) = -1 

    let ( === ) (t1: t) (t2: t) :bool = 
        (compare_alien_sym_list t1 t2 [] []) = 0

    let int_of_nat (t1: t) : int = 
       let rec add_syms (sym_lst: t) (acc: int) : int =
           match sym_lst with
            [] -> acc
           | hd::tl -> if (sum_overflows acc (M.int_of_aliensym(hd))) then (raise Unrepresentable)
                 else add_syms tl (Pervasives.(+) acc (M.int_of_aliensym(hd))) in
                     add_syms t1 0

    let nat_of_int (x: int) : t =
        let rec make_sym_list (counter: int) (acc: t) : t =
            if (counter = 0) then acc
            else make_sym_list (counter - 1) (M.one::acc) in
                 make_sym_list x [M.zero]
end 


module AlienConvert = AlienNatFn(M)


