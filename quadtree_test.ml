open Quadtree
open Assertions

let empty_t = new_tree ((-8.0, -8.0), (8.0, 8.0)) 
let one_object = insert empty_t (4.0, 4.0) 1 
let two_objects = insert one_object (-4.0, 4.0) 1
let three_objects = insert two_objects (-4.0, -4.0) 1
let four_objects = insert three_objects (4.0, -4.0) 1
let count_obj (acc : int) (tuple : coord * 'b) : int =
  acc + 1 

TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 one_object) = 1)
TEST_UNIT "insert_test2" = assert_true ((fold_quad count_obj 0 two_objects) = 2)
TEST_UNIT "insert_test1" = assert_true ((fold_quad count_obj 0 three_objects) = 3)
TEST_UNIT "insert_test2" = assert_true ((fold_quad count_obj 0 four_objects) = 4)