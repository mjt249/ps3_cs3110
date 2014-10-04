open Quadtree
open Assertions

let new_tree1 = new_tree((0., 0.), (0., 0.)) 
let new_tree2 = Leaf (((0., 0.), (0., 0.)), [])  
TEST_UNIT "new_tree_test1" = assert_true (new_tree1 = new_tree2)

let empty_t = new_tree ((-8.0, -8.0), (8.0, 8.0)) 
let one_object = insert empty_t (4.0, 4.0) 1 
let two_objects = insert one_object (-4.0, 4.0) 1
let three_objects = insert two_objects (-4.0, -4.0) 1
let four_objects = insert three_objects (4.0, -4.0) 1
let another_level = insert four_objects (2.0, 2.0) 1
let in_the_center = insert another_level (0.0, 0.0) 1
let min_diag_case = insert in_the_center (0.0, 0.0) 1
let count_obj (acc : int) (tuple : coord * 'b) : int =
  acc + 1 


(*testing insert requires using fold quad. 
  we are essentially testing fold quad here since my function count_obj
  passed through fold quad needs to give me the right number of insertions
  we also call quadtree functions in city search and we have tested those
  extensively as well.*)

TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 one_object) = 1)
TEST_UNIT "insert_test2" = assert_true ((fold_quad count_obj 0 two_objects) = 2)
TEST_UNIT "insert_test3" = assert_true ((fold_quad count_obj 0 three_objects) = 3)
TEST_UNIT "insert_test4" = assert_true ((fold_quad count_obj 0 four_objects) = 4)
TEST_UNIT "insert_test5" = assert_true ((fold_quad count_obj 0 another_level) = 5)
TEST_UNIT "insert_test6" = assert_true ((fold_quad count_obj 0 in_the_center) = 6)
TEST_UNIT "insert_test7" = assert_true ((fold_quad count_obj 0 min_diag_case) = 7)

TEST_UNIT "fold_region_test1" = 
  assert_true ((fold_region count_obj 0 one_object ((-8.0, -8.0), (0.0, 0.0)) = 0))
TEST_UNIT "fold_region_test2" = 
  assert_true ((fold_region count_obj 0 one_object ((-8.0, -8.0), (8.0, 8.0)) = 1))
  (*two objects are outside the region*)
TEST_UNIT "fold_region_test3" = 
  assert_true ((fold_region count_obj 0 four_objects ((-8.0, -8.0), (0.0, 8.0)) = 2))
TEST_UNIT "fold_region_test4" = 
  assert_true ((fold_region count_obj 0 four_objects ((-8.0, -8.0), (7.0, 8.0)) = 4))
TEST_UNIT "fold_region_test5" = 
  assert_true ((fold_region count_obj 0 in_the_center ((-5.0, -5.0), (0.0, 0.0)) = 2))
TEST_UNIT "fold_region_test6" =
  assert_true ((fold_region count_obj 0 min_diag_case ((-6.0, -6.0), (6.0, 6.0)) = 7))

let () = Pa_ounit_lib.Runtime.summarize() 