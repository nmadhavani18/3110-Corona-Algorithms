open OUnit
open Engine

let tests = [
  "test 1" >:: fun _ -> assert_equal "AAPL 282.97 7.94 2.89% : Apple Inc. - Yahoo Finance" (Engine.get_price "hi" 2)
      ~printer:(fun x -> x);
]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite