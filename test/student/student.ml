open OUnit2
open Basics

let test_sanity _ =
  assert_equal 1 1 ~msg:"Student Tests"
    
let test_rev_tup _ = 
  assert_equal (10,11) (rev_tup (11, 10)) ~msg:"rev_tup (5)";
  assert_equal ('a','b') (rev_tup ('b', 'a')) ~msg:"rev_tup (6)";
  assert_equal (1.0,5.0) (rev_tup (5.0, 1.0)) ~msg:"rev_tup (7)"

let test_rev_triple _ = 
  assert_equal (3.0, 4.0, 5.0) (rev_triple (5.0,4.0,3.0)) ~msg:"rev_triple (5)";
  assert_equal ('a','b','c') (rev_triple ('c','b','a')) ~msg:"rev_triple (6)";
  assert_equal (100,101,102) (rev_triple (102,101,100)) ~msg:"rev_triple (7)"

let test_is_odd _ = 
  assert_equal true (is_odd 9) ~msg:"is_odd (5)";
  assert_equal true (is_odd 11) ~msg:"is_odd (6)";
  assert_equal true (is_odd 13) ~msg:"is_odd (7)"

let test_pow _ = 
  assert_equal 8 (pow 2 3) ~msg:"pow (5)";
  assert_equal 1 (pow 1 8) ~msg:"pow (6)";
  assert_equal 16 (pow 4 2) ~msg:"pow (7)"

let suite =
  "student" >::: [
    "sanity" >:: test_sanity;
    "rev_tup" >:: test_rev_tup;
    "rev_trip" >:: test_rev_triple;
    "is_odd" >:: test_is_odd;
    "powow" >:: test_pow
  ]

let _ = run_test_tt_main suite
