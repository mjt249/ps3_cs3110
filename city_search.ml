open Str
open Parser
open Quadtree

(*s is of form name.typeOfFile*)
let load_city_data (s:string) : string quadtree = 
  let coor1 : coord = (-90.0, -180.0) in
  let coor2 : coord = (90.0, 180.0) in
  let city_region : region = (coor1,coor2) in
  let city_trees : string quadtree = new_tree city_region in
  let city_lists : city list = parse s in

  let rec insert_cities (lst_city : city list) (city_tree : string quadtree)  =
     match lst_city with
     [] -> city_tree
     | hd::tl -> 
       match hd with 
       (lat,long,name) -> let coor :coord = (lat,long) in
          insert_cities tl (insert city_tree coor name)
   in
 
   insert_cities city_lists city_trees

(*Preconditions: r need not be within the region of q
Postconditions: returns a list of the cities in the desired region of q*)
let city_search (q: string quadtree) (r : region) : string list = 
  let add_city_strings (lst: string list) 
    (city_tuple: coord * string) : string list =
    let city = snd city_tuple in
    let coord_of_city = fst city_tuple in
    let latitude = string_of_float(fst coord_of_city) in
    let longitude = string_of_float(snd coord_of_city) in
    (latitude^","^longitude^","^city)::lst in
  fold_region add_city_strings [] q r
