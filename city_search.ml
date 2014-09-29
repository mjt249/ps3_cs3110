open Str
open Parser
open Quadtree

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


let city_search (q: string quadtree) (r : region) : string list = 
	failwith "TODO"
