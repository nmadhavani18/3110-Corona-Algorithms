open OUnit
open Engine

(** TEST PLAN
    Most of our testing was performed manually through "make play", because the
    prices of stocks change every day, we felt that that using automated unit tests
    would be ineffective for our numerical outputs as most were based on 
    constantly changing numbers.  *)
let data = data_processor (data_lines "transactions.txt") [] 
let tests = [
  "test save" >:: (fun _ -> assert_equal () (Engine.save_file "AAPL"));
  "test url" >:: (fun _ -> assert_equal "https://www.marketbeat.com/stocks/NASDAQ/AAPL/" (Engine.get_url "AAPL"));
  "test file" >:: (fun _ -> assert_equal "html/AAPL.html" (Engine.get_file "AAPL"));
  (* ("test body" >:: fun _ -> assert_equal "AAPL.html" (Engine.file "AAPL")); *)
  "test price" >:: (fun _ -> assert_equal 620.26 (Engine.get_price "AAPL" 2));
  "test no shares" >:: (fun _ -> 
      assert_equal 0 (Engine.shares_search "LPTH" data));


]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite