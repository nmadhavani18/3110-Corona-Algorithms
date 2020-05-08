open OUnit
open Engine

let tests = [
  ("test price" >:: fun _ -> assert_equal 303.74 (Engine.get_price "hi" 2));
  ("test url" >:: fun _ -> assert_equal "https://www.marketbeat.com/stocks/NASDAQ/AAPL/" (Engine.get_url "AAPL"));
  ("test file" >:: fun _ -> assert_equal "AAPL.html" (Engine.get_file "AAPL"));
  ("test body" >:: fun _ -> assert_equal "AAPL.html" (Engine.file "AAPL"));
]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite