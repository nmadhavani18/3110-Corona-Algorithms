open OUnit
open Engine

let tests = [
  ("test url" >:: fun _ -> assert_equal "https://www.marketbeat.com/stocks/NASDAQ/AAPL/" (Engine.get_url "AAPL"));
  ("test file" >:: fun _ -> assert_equal "html/AAPL.html" (Engine.get_file "AAPL"));
  (* ("test body" >:: fun _ -> assert_equal "AAPL.html" (Engine.file "AAPL")); *)
  ("test save" >:: fun _ -> assert_equal () (Engine.save_file "AAPL"));
  ("test price" >:: fun _ -> assert_equal 620.26 (Engine.get_price "AAPL" 2));

]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite