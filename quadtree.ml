type coord = float * float
type region = coord * coord
(* A Node is represented by a region and four quadtrees that divide the
 region. 
 A Leaf is represented by a region and a list of coord * 'a tuple which
 represent the object in the Leaf's region.

 A region consists of two coords which represent the lower left and upper
 right corners of the square it represents. Coords is a float tuple of 
 and x and y.*)
type 'a quadtree =
  Node of region * 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
  | Leaf of region * ((coord * 'a) list)
		       
let min_diagonal = 0.0000001
		     
exception OutOfBounds


(*returns: a quad tree that is a leaf with given region r and no objects.*)
let new_tree (r:region) : 'a quadtree = 
  Leaf (r, [])
        
(*1) turn leaf to node
2) which quad is A in? for First object
3) which quat B  is in? Second object
4) if A and B in same quad, repeat 1-3 with new quad
5) else add to empty respective quad leaves
*)

let insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let object_is_in_bounds (an_r : region) (a_c : coord) : bool = 
    ((fst (fst an_r) <= fst a_c) && (snd (fst an_r) <= snd a_c) &&
    (fst (snd an_r) >= fst a_c) && (snd (snd an_r) >= snd a_c)) in

  let quadrent_from_region (one: 'a quadtree) (two: 'a quadtree) (three: 'a quadtree) (coordnant: coord) : string =
     if object_is_in_bounds (fst one) c then
        "I"
     else if object_is_in_bounds (fst two) c then
        "II"
     else if object_is_in_bounds (fst three) c then
        "III"
     else
        "IV"
     in
 
  let rec two_objects_dilema (reg: region) (A_object: coord*'a) (B_object: coord*'a) : 'a quadtree =
       

  let insert_to_leaf (leaf: 'a quadtree) (obj_coord : coord) (obj: 'a) :'a quadtree =
    let leaf_region = fst(leaf) in
    let x0 = fst(fst leaf_region) in
    let x1 = fst(snd leaf_region) in
    let y0 = snd(fst leaf_region) in
    let y1 = snd(snd leaf_region) in
    let leaf_diag = sqrt((x1 - x0)**2.0 + (y1 - y0)**2.0) in
    if (leaf_diag <= min_diagonal || List.length(snd leaf) <1) then
       Leaf( fst(leaf), ((obj_coord, s)::snd(leaf)) )
    else
      





  if object_is_in_bounds (fst q) c then

  let rec find_leaf (current_quadrent: 'a quadtree) : 'a quadtree =
    match q with
    | Leaf (r, coord_lst) -> insert_to_leaf q c s
    | Node (r, one, two, three, four) ->
         (match (quadrent_from_region one two three) with
          |"I" -> Node(r, (find_leaf one), two, three, four) 
          |"II" -> Node(r, one, (find_leaf two), three, four) 
          |"III" ->  Node(r, one, two, (find_leaf three), four)
          | _ ->  Node(r, one, two, three, (find_leaf four))
        in)
    in



   find_leaf(q)

  else raise OutOfBounds 
  

  
							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  =
  failwith "TODO"
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  failwith "TODO"

