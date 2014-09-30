open City_search
open Quadtree
open Assertions


let ithaca_tree = load_city_data "ithaca.csv"
let count_obj (acc : int) (tuple : coord * 'b) : int =
  acc + 1 



TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 ithaca_tree) = 7)
