open OUnit
open Engine

let tests = [
  "test 1" >:: fun _ -> assert_equal "282.97" (Engine.get_price "hi" 2);
]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite