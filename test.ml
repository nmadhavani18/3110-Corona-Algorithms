open OUnit
open Engine

let tests = [
  ("test 1" >:: fun _ -> assert_equal 288.79 (Engine.get_price "hi" 2));
  ("test url" >:: fun _ -> assert_equal "https://www.marketbeat.com/stocks/NASDAQ/AAPL/" (Engine.get_url "AAPL"));
]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite