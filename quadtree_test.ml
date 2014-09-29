open Quadtree
open Assertions

let empty_t = new_tree ((-8.0, -8.0), (8.0, 8.0)) 
let one_object = insert empty_t (4.0, 4.0) 1 
let two_objects = insert one_object (-4.0, 4.0) 1
let three_objects = insert two_objects (-4.0, -4.0) 1
let four_objects = insert three_objects (4.0, -4.0) 1
let another_level = insert four_objects (2.0, 2.0) 1
let in_the_center = insert another_level (0.0, 0.0) 1
let count_obj (acc : int) (tuple : coord * 'b) : int =
  acc + 1 

TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 one_object) = 1)
TEST_UNIT "insert_test2" = assert_true ((fold_quad count_obj 0 two_objects) = 2)
TEST_UNIT "insert_test3" = assert_true ((fold_quad count_obj 0 three_objects) = 3)
TEST_UNIT "insert_test4" = assert_true ((fold_quad count_obj 0 four_objects) = 4)
TEST_UNIT "insert_test5" = assert_true ((fold_quad count_obj 0 another_level) = 5)
TEST_UNIT "insert_test6" = assert_true ((fold_quad count_obj 0 in_the_center) = 6)

TEST_UNIT "fold_region_test1" = 
  assert_true ((fold_region count_obj 0 one_object ((-8.0, -8.0), (0.0, 0.0)) = 0))
TEST_UNIT "fold_region_test2" = 
  assert_true ((fold_region count_obj 0 one_object ((-8.0, -8.0), (8.0, 8.0)) = 1))
TEST_UNIT "fold_region_test3" = 
  assert_true ((fold_region count_obj 0 four_objects ((-8.0, -8.0), (0.0, 8.0)) = 2))
TEST_UNIT "fold_region_test4" = 
  assert_true ((fold_region count_obj 0 four_objects ((-8.0, -8.0), (7.0, 8.0)) = 4))
TEST_UNIT "fold_region_test5" = 
  assert_true ((fold_region count_obj 0 in_the_center ((-5.0, -5.0), (0.0, 0.0)) = 2))