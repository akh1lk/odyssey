open OUnit2
open Odyssey

(* Helper function to test parsing and evaluation *)
let test_parse_and_eval name expr data expected_eval _ =
  Printf.printf "Testing: %s\n" name;
  Printf.printf "Expression: %s\n" expr;
  let parsed = PropEval.parse_prop expr in
  Printf.printf "Parsed: %s\n" (PropEval.print_prop parsed);
  let eval_result = PropEval.eval_prop parsed (PropEval.create_data data) in
  Printf.printf "Evaluation Result: %b\n" eval_result;
  assert_equal expected_eval eval_result

(* Simplified test cases *)
let suite =
  "Simplified Proposition Tests"
  >::: [
         "Test x as true"
         >:: test_parse_and_eval "x as true" "x" [ "x true" ] true;
         "Test ~x as false"
         >:: test_parse_and_eval "~x as false" "~x" [ "x true" ] false;
         "Test (x ^ z) as true"
         >:: test_parse_and_eval "(x ^ z) as true" "(x ^ z)"
               [ "x true"; "z true" ] true;
         "Test (x v y) as true"
         >:: test_parse_and_eval "(x v y) as true" "(x v y)"
               [ "x true"; "y false" ] true;
         "Test (x => z) as true"
         >:: test_parse_and_eval "(x => z) as true" "(x => z)"
               [ "x true"; "z true" ] true;
         "Test (x ^ (~y)) as false"
         >:: test_parse_and_eval "(x ^ (~y)) as false" "(x ^ (~y))"
               [ "x true"; "y true" ] false;
       ]

(* Run tests *)
let () = run_test_tt_main suite
