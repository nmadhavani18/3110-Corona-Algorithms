open OUnit
open Engine
open Command

(** TEST PLAN
    Most of our testing was performed manually through "make play", because the
    prices of stocks change constantly, we felt that that using automated 
    unit tests would be ineffective for our numerical outputs as most were based
    on constantly changing numbers. The algorithms (mean_reversion, threshold, 
    etc.) cannot be tested by OUnit testing, since they are dependent on the 
    precise stock prices at any given time. However, we used asser_bool 
    functions in order to test our get_price function using bounds that stock 
    prices were highly unlikely to cross. Additionally, many functions were 
    called several times in other locations so the best way to ensure 
    correctness was to look at the final output of a series of operations, 
    rather than trying to write a test case for a single function. In order to 
    test the precise pricing data or testing to ensure that the webscraping 
    functions in engine.ml were correct, we had to manually cross-check the 
    data that our engine pulled with Google. We mostly used randomized testing
    in order to thoroughly check that the program was working. We used a 
    random stock generator (https://raybb.github.io/random-stock-picker/) to 
    make sure that the program was not using preexisting local html files to 
    pull data.*)
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
  "test NASDAQ price" >:: (fun _ -> 
      assert_bool "AAPL Price < 600" ((Engine.get_price "AAPL" 1) < 600.00));
  "test NASDAQ price" >:: (fun _ -> 
      assert_bool "AAPL Price > 100" ((Engine.get_price "AAPL" 1) > 100.00));
  "test NYSE price" >:: (fun _ -> 
      assert_bool "NKE Price < 250" ((Engine.get_price "NKE" 1) < 250.00)); 
  "test NYSE price" >:: (fun _ -> 
      assert_bool "NKE Price > 10" ((Engine.get_price "NKE" 1) > 10.00));  
  "test NASDAQ penny stock price" >:: (fun _ -> 
      assert_bool "ALRN Price < 15" ((Engine.get_price "ALRN" 1) < 15.00));
  "test NASDAQ penny stock price" >:: (fun _ -> 
      assert_bool "ALRN Price > 0.001" ((Engine.get_price "ALRN" 1) > 0.001));
  "test NYSE penny stock price" >:: (fun _ -> 
      assert_bool "TRQ Price > 0.001" ((Engine.get_price "TRQ" 1) > 0.001));
  "test NYSE penny stock price" >:: (fun _ -> 
      assert_bool "TRQ Price < 5" ((Engine.get_price "TRQ" 1) < 5.00));
  "test multiple shares NASDAQ price" >:: (fun _ -> 
      assert_bool "2 AAPL Price < 1200" ((Engine.get_price "AAPL" 2) < 1200.00));
  "test multiple shares NASDAQ price" >:: (fun _ -> 
      assert_bool "2 AAPL Price > 100" ((Engine.get_price "AAPL" 2) > 200.00));
  "test multiple shares NYSE price" >:: (fun _ -> 
      assert_bool "2 NKE Price < 250" ((Engine.get_price "NKE" 2) < 500.00)); 
  "test multiple shares NYSE price" >:: (fun _ -> 
      assert_bool "2 NKE Price > 20" ((Engine.get_price "NKE" 2) > 20.00));  
  "test multiple shares NASDAQ penny stock price" >:: (fun _ -> 
      assert_bool "2 ALRN Price < 30" ((Engine.get_price "ALRN" 2) < 30.00));
  "test multiple shares NASDAQ penny stock price" >:: (fun _ -> 
      assert_bool "2 ALRN Price > 0.001" ((Engine.get_price "ALRN" 2) > 0.001));
  "test multiple shares NYSE penny stock price" >:: (fun _ -> 
      assert_bool "2 TRQ Price > 0.001" ((Engine.get_price "TRQ" 2) > 0.001));
  "test multiple shares NYSE penny stock price" >:: (fun _ -> 
      assert_bool "2 TRQ Price < 10" ((Engine.get_price "TRQ" 2) < 10.00));
  "test no existing shares" >:: (fun _ -> 
      assert_equal 0 (Engine.shares_search "LPTH" data));
  "test buy record" >:: (fun _ -> 
      assert_equal "bought 10 shares of NFLX at 450.32 on 2020-05-01"
        (Engine.record "bought" "NFLX" 10 450.32 "2020-05-01"));
  "test sell record" >:: (fun _ ->
      assert_equal "sold 4 shares of GOOGL at 523.18 on 2020-05-01"
        (Engine.record "sold" "GOOGL" 4 523.18 "2020-05-01"));
  "test compare true" >:: (fun _ ->
      assert_equal true (Engine.compare 351.58 298.2));
  "test compare false" >:: (fun _ -> 
      assert_equal false (Engine.compare 14.41 303.0));
  "test compare equal" >:: (fun _ -> 
      assert_equal true (Engine.compare 14.41 14.41));
]

let suite =
  "test suite for project"  >::: List.flatten [
    tests;
  ]

let _ = run_test_tt_main suite
