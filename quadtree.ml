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
exception PreconditionNotMet of string

(*returns: a quad tree that is a leaf with given region r and no objects.*)
let new_tree (r:region) : 'a quadtree = 
  Leaf (r, [])

(*insert object s into quadtree q at coor c*)
let insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  (*returns the region*)
  let get_region (a_q_tree : 'a quadtree) : region =
    match  a_q_tree with
     Node (an_r, _, _, _, _) -> an_r
    |Leaf (an_r, _) -> an_r in
  (*requires: leaf
  returns coord*obj list*)
  let get_obj_list (a_q_tree : 'a quadtree) : ((coord * 'a) list) =
    match  a_q_tree with
     Node (_, _, _, _, _) -> raise (PreconditionNotMet "get_obj_list wants a Leaf")
    |Leaf (_, lst) -> lst in
  (*returns coords are in bounds of region.*)  
  let object_is_in_bounds (an_r: region) (a_c : coord) : bool = 
    ((fst (fst an_r) <= fst a_c) && (snd (fst an_r) <= snd a_c) &&
    (fst (snd an_r) >= fst a_c) && (snd (snd an_r) >= snd a_c)) in

  (*requires: a node
  * returns which quad the coords are in*)
  let quad_of_coords (node: 'a quadtree) (coords: coord) : string =
    match node with
    Node (r, one, two, three, four) ->
      if object_is_in_bounds (get_region one) c then
        "I"
      else if object_is_in_bounds (get_region two) c then
        "II"
      else if object_is_in_bounds (get_region three) c then
        "III"
      else
        "IV"  
    |Leaf ( _, _) -> raise (PreconditionNotMet "quad_of_coords wants a Node") in

  (*I made insert_to_leaf recursive because we need that base case again. 
    basically the else clause still has what we wanted for "two_objects_dilemma 
    but insert_to_leaf is the recursive function because we need to check for the
    diagonal each time."*)
  (*if leaf has less than mindiag or no obj, insert. otherwise, split it up one level.*)
  let rec insert_to_leaf (leaf: 'a quadtree) (obj_coord : coord) (obj: 'a) :'a quadtree =
    let leaf_region = get_region leaf in
    let x0 = fst(fst leaf_region) in
    let x1 = fst(snd leaf_region) in
    let y0 = snd(fst leaf_region) in
    let y1 = snd(snd leaf_region) in
    let leaf_diag = sqrt((x1 -. x0)**2.0 +. (y1 -. y0)**2.0) in
    (*leaf has no objects, or can't be split up anymore.*)
    if (leaf_diag <= min_diagonal || List.length(get_obj_list leaf) <1) then
       Leaf( (get_region leaf), ((obj_coord, s)::(get_obj_list leaf)))
    (*leaf has an object and can be split up*)
    else
      (*make a node with four leafs out of the original leaf. no objects are in it.*)
      let new_node = Node (leaf_region, 
      new_tree(((x0 +. x1) /. 2.0, (y0 +. y1) /. 2.0), (x1, y1)), 
      new_tree((x0, (y0 +. y1) /. 2.0), ((x0 +. x1) /. 2.0, y1)), 
      new_tree((x0, y0), ((x0 +. x1) /. 2.0, (y0 +. y1) /. 2.0)), 
      new_tree(((x0 +. x1) /. 2.0, y0), ((x1, (y0 +. y1) /. 2.0)))) in
      (*the coords * object tuple*)
      let tuple_in_leaf = List.hd(get_obj_list leaf) in
      (*the strings that represent the quad that the objects are in*)
      let quad_of_original_obj = quad_of_coords new_node (fst(tuple_in_leaf)) in
      let quad_of_inserting_obj =  quad_of_coords new_node obj_coord in
      (*inserts the original object in the right quadtree leaf
        requires a Node with four leaves*)
      let original_obj_inserted (node: 'a quadtree) (quad_string: string)= 
        match node with
         Leaf (_, _) -> raise (PreconditionNotMet "original_obj_inserted wants a Node") 
        |Node (reg, one, two, three, four) -> 
          match quad_string with
          |"I" -> Node (reg, Leaf ((get_region one), [tuple_in_leaf]), two, three, four)
          |"II" -> Node (reg, one, Leaf ((get_region two), [tuple_in_leaf]), three, four)
          |"III" -> Node (reg, one, two, Leaf ((get_region three), [tuple_in_leaf]), four)
          | _ -> Node (reg, one, two, three, Leaf ((get_region four), [tuple_in_leaf])) in
      (*the node now has the original object inserted in the right leaf*)
      let node_w_orig_obj = original_obj_inserted new_node quad_of_original_obj in
      (*match statement to bind the four leaves of the node*)
      match node_w_orig_obj with
       Leaf (_, _) -> raise (PreconditionNotMet "else block of insert_to_leaf wants a Node") 
      |Node (reg, one, two, three, four) ->
          (*there is no longer a reason to check if they conflict or not. 
            insert_to_leaf does that for us in the if statement (check if leaf has
            and object already.) so i just call insert_to_leaf on the right leaf.*)
        match quad_of_inserting_obj with
        |"I" -> Node (reg, (insert_to_leaf one obj_coord obj), two, three, four)
        |"II" -> Node (reg, one, (insert_to_leaf two obj_coord obj), three, four)
        |"III" -> Node (reg, one, two, (insert_to_leaf three obj_coord obj), four)
        | _ -> Node (reg, one, two, three, (insert_to_leaf four obj_coord obj))in

  if object_is_in_bounds (get_region q) c then
  (*obj is in bounds so find the leaf it is a part of.*)
  let rec find_leaf (current_quadrent: 'a quadtree) : 'a quadtree =
    match q with
      Leaf (r, coord_lst) -> insert_to_leaf q c s
    | Node (r, one, two, three, four) ->
         (match (quad_of_coords q c) with
          |"I" -> Node(r, (find_leaf one), two, three, four) 
          |"II" -> Node(r, one, (find_leaf two), three, four) 
          |"III" ->  Node(r, one, two, (find_leaf three), four)
          | _ ->  Node(r, one, two, three, (find_leaf four)) ) in

   find_leaf(q)

  else raise OutOfBounds 
  
					      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
=
  match t with
   Leaf (reg, obj_lst) -> List.fold_left f a obj_lst
  |Node (reg, one, two, three, four) -> 
    fold_quad f ( fold_quad f ( fold_quad f (fold_quad f a four) three ) two ) one

let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  let acc_obj_tuple (acc: (coord * 'b) list) (obj_tuple: coord * 'b) : (coord * 'b) list =
    obj_tuple :: acc in
  let all_obj_lst = fold_quad acc_obj_tuple [] t in

  let tuple_is_in_bounds (an_r: region) (a_tuple: coord * 'b) : bool =
    let a_c = fst a_tuple in 
    ((fst (fst an_r) <= fst a_c) && (snd (fst an_r) <= snd a_c) &&
    (fst (snd an_r) >= fst a_c) && (snd (snd an_r) >= snd a_c)) in
    
  let objects_within_r = List.filter (tuple_is_in_bounds r) all_obj_lst in

  List.fold_left f a objects_within_r

