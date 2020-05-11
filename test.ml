open OUnit
open Engine
open Command

(** TEST PLAN
    Most of our testing was performed manually through "make play", because the
    prices of stocks change constantly, we felt that that using automated 
    unit tests would be ineffective for our numerical outputs as most were based 
    on constantly changing numbers. Additionally, many functions were called
    several times in other locations so the best way to ensure correctness
    was to look at the final output of a series of operations, rather than
    trying to write a test case for a single function.  *)
let data = data_processor (data_lines "transactions.txt") [] 
let tests = [
  "test save" >:: (fun _ -> assert_equal () (Engine.save_file "AAPL"));
  "test save2" >:: (fun _ -> assert_equal () (Engine.save_file "BAC"));
  "test save3" >:: (fun _ -> assert_equal () (Engine.save_file "V"));
  "test NASDAQ url1" >:: (fun _ -> 
      assert_equal "https://www.marketbeat.com/stocks/NASDAQ/AAPL/" 
        (Engine.get_url "AAPL"));
  "test NASDAQ url2" >:: (fun _ ->
      assert_equal "https://www.marketbeat.com/stocks/NASDAQ/FB/"
        (Engine.get_url "FB"));
  "test NYSE url1" >:: (fun _ ->
      assert_equal "https://www.marketbeat.com/stocks/NYSE/BAC/" 
        (Engine.get_NYSE_url "BAC"));
  "test NYSE url2" >:: (fun _ ->
      assert_equal "https://www.marketbeat.com/stocks/NYSE/NKE/" 
        (Engine.get_NYSE_url "NKE"));
  "test file" >:: (fun _ -> 
      assert_equal "html/AAPL.html" (Engine.get_file "AAPL"));
  "test file2" >:: (fun _ ->
      assert_equal "html/MCD.html" (Engine.get_file "MCD"));
  (* ("test body" >:: fun _ -> assert_equal "AAPL.html" (Engine.file "AAPL")); *)
  (* "test price" >:: (fun _ -> assert_equal 620.26 (Engine.get_price "AAPL" 2)); *)
  "test no existing shares" >:: (fun _ -> 
      assert_equal 0 (Engine.shares_search "LPTH" data));
  "test buy record" >:: (fun _ -> 
      assert_equal "bought 10 shares of NFLX at 450.32 on 2020-05-01"
        (Engine.record "bought" "NFLX" 10 450.32 "2020-05-01"));
  "test sell record" >:: (fun _ ->
      assert_equal "sold 4 shares of GOOGL at 523.18 on 2020-05-08"
        (Engine.record "sold" "GOOGL" 4 523.18 "2020-05-08"));
  "test compare true" >:: (fun _ ->
      assert_equal true (Engine.compare 351.58 298.2));
  "test compare false" >:: (fun _ -> 
      assert_equal false (Engine.compare 14.41 303.0));



]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite
