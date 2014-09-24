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
        
let rec insert (q: 'a quadtree) (c : coord) (s:'a) : 'a quadtree =
  let object_is_in_bounds (an_r : region) (an_c : coord) = 
    
  if  

  
							      
let rec fold_quad (f: 'a -> (coord * 'b)  -> 'a)
		  (a: 'a) (t: 'b quadtree): 'a 
  =
  failwith "TODO"
	   
let rec fold_region (f: 'a -> coord * 'b -> 'a) (a : 'a) (t : 'b quadtree) 
  (r : region) : 'a
=
  failwith "TODO"

