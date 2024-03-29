(* BEGIN: DO NOT CHANGE THIS CODE, except for adding comments
   to NATN as required in exercise 1. *)
module type NATN = sig
    (*t is a representation of a natural number or a number >= 0*)
    type t

    (*zero has the identity property that zero + t = t and zero * t = zero*)
    val zero : t

    (*one has the identity property that 1 * t = 1 unless t is 0*)
    val one : t

   (*(+) takes two ts and returns a t representation of their sum
   * (+) is associative: (a + b) + c = a + (b + c)
   * (+) is commutative: a + b = b + a
   * Identity is 0 --> 0 + t = t *)
    val ( + ) : t -> t -> t

   (*( * )takes two ts and returns a t representation of their product
   * ( * ) is associative: (a * b) * c = a * (b * c)
   * ( * ) is commutative: a * b = b * a
   * Identity is 1 -> 1 * t = t
   * multiplication is distributive over addition:
   * a * (b + c) = (a * b) + (a * c) *)
    val ( * ) : t -> t -> t 

   (*( < ) returns true only if the first t passed is less than the second t*)
    val ( < ) : t -> t -> bool
   (*( === ) returns true only if the first t 
   *and the second t represent the same natural number *)
    val ( === ) : t -> t -> bool
   (* Unrepresentable if called if the t passed int_of_nat would be 
   *more than max_int. It is called on nat_of_int if a negative int is passed*)
    exception Unrepresentable
   (*int_of_nat returns an int representation of t.
   * Must raise Unrepresentable if t > than max_int*)
    val int_of_nat: t -> int
   (*nat_of_int returns a t representation of the natural int passed
   *Must raise Unrepresentable if int < 0*)
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
    (*will never raise Unrepresentable because x is implemented with
      t = int. x must a natural number be less than max_int*)
    let int_of_nat (x : t) : int= 
        x

    let nat_of_int (x : int) : t= 
        if x < 0 then (raise Unrepresentable)
        else x

    let zero : t = 0
    let one : t = 1
  
  (*neither of the two arguments have experienced overflow themselves.*)
  (*raise Unrepresentable if result is larger than max_int*)
    let ( + ) (t1: t) (t2: t) : t =

        let int_t1 = int_of_nat t1 in
        let int_t2 = int_of_nat t2 in
            if (sum_overflows int_t1 int_t2) then (raise Unrepresentable)
            else
                nat_of_int(int_t1 + int_t2)

  (*neither of the two arguments have experienced overflow thenselves.*)
  (*raise Unrepresentable if result is larger than max_int*)

    let ( * ) (t1: t) (t2: t) : t = 
        if (t1 = zero || t2 = zero) then zero
        else
            let rec multiply_helper (fst: t) (snd: t) (prod: t) : t = 
                if (fst = zero) then prod
                else
                    multiply_helper 
                      (nat_of_int((int_of_nat fst) -1)) snd (snd + prod) in
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
  (*If x is longer that max_int, will raise unrepresentable.*)
    let int_of_nat (x : t) : int =
        let rec int_of_nat_helper (lst: t) (counter: int) : int =
            if (counter < 0) then (raise Unrepresentable)
            else  
                match lst with
                 [] -> counter
                |hd::tail -> 
                   int_of_nat_helper tail (Pervasives.(+) counter 1) in
                      int_of_nat_helper x 0
    (*cons on another int to lst. Does this current times*)
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
            |hd::tail -> 
              product_helper tail second (add_list_to_length t2 product) in
                product_helper t1 t2 []

    (*If returned is 0, then equal, if 1, then second is longer, else 
    *first is longer*)
    let rec compare_length (t1: t) (t2: t) : int =
        match t1, t2 with
         [], [] -> 0 (*lists even*)
        | [], hd::tl -> 1 (*second list longer*)
        | hd::tl, [] -> -1 (*first list longer*)
        | hd1::tl1, hd2::tl2 -> compare_length tl1 tl2  

     let ( < ) (t1: t) (t2: t) : bool =
        (compare_length t1 t2) = 1

    let ( === ) (t1: t) (t2: t) : bool =
        (compare_length t1 t2) = 0
end

module NatConvertFn ( N : NATN ) = struct
    (*converts natural numbers of type N.t to an int*)
    let int_of_nat (n : N.t ): int = N.int_of_nat(n)

    (*converts ints to natural numbers of type N.t*)
    let nat_of_int (n : int ): N.t = N.nat_of_int(n) 
       
(* KARMA. compiles.
    (*returns binary number system representation of n*)
    let two = N.(+) N.one N.one 
    let nat_of_int (n: int): N.t =
      let rec binary (n: int) (acc: N.t list) : N.t list= 
        if (n = 0) then acc
        else if (n = 1) then (N.one::acc)
        else 
          if ((n mod 2) = 0 ) then binary (n/2) (N.zero::acc)
          else
            binary ((n-1)/2) (N.one::acc) in
    (*returns natural number of the given binary representation of the number.*)
      let rec binary_to_nat (binary: N.t list) (acc: N.t) (digit: N.t) : N.t=
        let bi = List.rev(binary) in
        match bi with
         [] -> acc
        |hd::tl -> if (N.(===) hd (N.one)) then 
                     binary_to_nat tl (N.(+) acc digit) (N.( * ) digit two)
                   else 
                     binary_to_nat tl acc (N.( * ) digit two) in
      let binary_rep = binary(n) [] in
      binary_to_nat binary_rep N.zero N.one

    let int_of_nat (n : N.t ): int = 
      (*returns int of natural number n. description in written part.*)
      let rec interval_finder (wanted: N.t) (low: N.t) (current: N.t) (digit: N.t) 
          (current_int: int) (digit_int : int) (restart: int) : int
      =
        if (N.(===) wanted N.zero) then 0
        else if (N.(===) wanted current) then current_int
        else if (N.(<) current wanted) then
          interval_finder wanted current (N.(+) current digit) (N.( * ) digit two)
            (current_int + digit_int) (digit_int*2) current_int
        else if (N.(<) wanted current) then
          interval_finder wanted low (N.(+) low N.one) (N.one)
            (restart + 1) 1 restart
        else 
          failwith "impossible" in
      interval_finder n N.zero N.one N.one 1 1 0 *)

end


module AlienNatFn (M: AlienMapping): NATN = struct
    type t = M.aliensym list
    exception Unrepresentable
    let zero : t = [M.zero]
    let one : t = [M.one]

    let add_helper (acc : t) (el : M.aliensym) = el::acc
    let sub_helper (acc: int list) (el: M.aliensym) = 
        (-M.int_of_aliensym(el))::acc
    let other_add_helper (acc: int list) (el: M.aliensym) = 
        (M.int_of_aliensym(el))::acc

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
        | hd::tl -> 
              prod_helper first tl (acc + (sym_list_product first hd)) in
                prod_helper t1 t2 []

    (*The next three methods work to fold a list of two appended lists to
    *determine which is more or if they are even*)
    let create_list_to_check (first: t) (second: t) : int list=
        let first_part = List.fold_left (fun acc e -> 
           (sub_helper acc e)) [] first in
        List.fold_left (fun acc e -> 
           (other_add_helper acc e)) first_part second

    let overflow_checker (ac: int) (el: int) (overflow:int list):int*int list =
        let sum = Pervasives.(+) ac el in
        match ((ac >= 0),(el >= 0),(sum >= 0)) with
         (true, true, false) -> ((sum - max_int), (1::overflow))
        | (false, false, true) -> ((Pervasives.(+) sum max_int), -1::overflow)
        | (_,_,_) -> (sum, overflow)

    let rec compare_list (accm,lst_of_ints) : int =
        if (lst_of_ints = []) then accm
        else
            let acc : int * int list = (0,[]) in
            compare_list (List.fold_left (fun acc e ->
               (overflow_checker (fst acc) e (snd acc))) acc lst_of_ints) 

    let ( < ) (t1: t) (t2: t) :bool=
        let lst = create_list_to_check t1 t2 in
        let rslt :int = compare_list (0,lst) in
        (rslt > 0)

    let ( === ) (t1: t) (t2: t) :bool = 
        let lst = create_list_to_check t1 t2 in
        let rslt = compare_list (0,lst) in
        (rslt = 0)

    let int_of_nat (t1: t) : int = 
        let rec add_syms (sym_lst: t) (acc: int) : int =
            match sym_lst with
             [] -> acc
            | hd::tl -> 
                if (sum_overflows acc (M.int_of_aliensym(hd))) then 
                    (raise Unrepresentable)
                else add_syms tl (Pervasives.(+) acc (M.int_of_aliensym(hd))) in
        add_syms t1 0
   
    let nat_of_int (x: int) : t =
        if (x >= 0) then 
            let rec make_sym_list (counter: int) (acc: t) : t =
                if (counter = 0) then acc
            else make_sym_list (counter - 1) (M.one::acc) in
            make_sym_list x []
        else 
            (raise Unrepresentable)

end 

(*For testing purposes only. Specifically for AlienNatFn.*)
(*module M : AlienMapping = struct
    type aliensym = float
    let int_of_aliensym (a : aliensym) : int = int_of_float a
    let one : aliensym = 1.0
    let zero : aliensym = 0.0
end 


module AlienConvert = AlienNatFn(M) *)


