open City_search
open Quadtree
open Assertions


let ithaca_tree = load_city_data "ithaca.csv"
let count_obj (acc : int) (tuple : coord * 'b) : int =
  acc + 1 

let test_region_1 : region = (((-90.0),(-180.0)),(90.0,180.0))


TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 ithaca_tree) = 7)

TEST_UNIT "City_search" = List.iter print_string (city_search ithaca_tree test_region_1)