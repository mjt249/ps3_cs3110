open City_search
open Quadtree
open Assertions


let ithaca_tree = load_city_data "ithaca.csv"
let count_obj (acc : int) (tuple : coord * 'b) : int =
  acc + 1 
let city_tree = load_city_data "city_file.csv"
let test_region_All : region = ((-90.0,-180.0),(90.0,180.0))
let test_region_South_East : region = ((0.0,-180.0),(90.0, 0.0))
let test_region_North_West : region = ((-90.0,0.0),(0.0, 180.0))
let test_region_North_East : region = ((0.0,0.0),(90.0, 180.0))
let test_region_South_West : region = ((-90.0,-180.0),(0.0, 0.0))
let test_region_Middle : region = ((-45.0,-45.0),(45.0,45.0))
let test_region_Outside : region = ((-900.0,-1800.0),(900.0,1800.0))

TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 ithaca_tree) = 7)

(* TEST_UNIT "City_search" = List.iter print_string (city_search city_tree test_region_All) *)
TEST_UNIT "City_search_1" = assert_true (List.length(city_search city_tree test_region_All) = 3)
TEST_UNIT "City_search_2" = assert_true (List.length(city_search city_tree test_region_South_East) = 1)
TEST_UNIT "City_search_3" = assert_true (List.length(city_search city_tree test_region_North_West) = 1)
TEST_UNIT "City_search_4" = assert_true (List.length(city_search city_tree test_region_North_East) = 2)
TEST_UNIT "City_search_5" = assert_true (List.length(city_search city_tree test_region_South_West) = 2)
TEST_UNIT "City_search_5" = assert_true (List.length(city_search city_tree test_region_Middle) = 1)
TEST_UNIT "City_search_1" = assert_true (List.length(city_search city_tree test_region_Outside) = 3)