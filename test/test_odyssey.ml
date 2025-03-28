open OUnit2
open Odyssey

(* Helper function to create tests for PropEval.eval_prop *)
let make_test name prop_str data_lst expected =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let data = PropEval.create_data data_lst in
  let result = PropEval.eval_prop prop data in
  assert_equal expected result ~printer:string_of_bool

let suite =
  "PropEval Tests"
  >::: [
         (* Variables *)
         make_test "Test var true" "x" [ "x true" ] true;
         make_test "Test var false" "x" [ "x false" ] false;
         (* NOT ( ~ )*)
         make_test "Test not true" "~x" [ "x false" ] true;
         make_test "Test not false" "~x" [ "x true" ] false;
         make_test "Test double not" "~(~x)" [ "x true" ] true;
         (* AND ( ^ ) *)
         make_test "Test and true" "x ^ y" [ "x true"; "y true" ] true;
         make_test "Test and false 1" "x ^ y" [ "x false"; "y true" ] false;
         make_test "Test and false 2" "x ^ y" [ "x true"; "y false" ] false;
         (* OR ( v ) *)
         make_test "Test or true 1" "x v y" [ "x true"; "y false" ] true;
         make_test "Test or true 2" "x v y" [ "x false"; "y true" ] true;
         make_test "Test or false" "x v y" [ "x false"; "y false" ] false;
         (* IMPLIES ( -> ) *)
         make_test "Test implies true 1" "x -> y" [ "x true"; "y true" ] true;
         make_test "Test implies true 2" "x -> y" [ "x false"; "y false" ] true;
         make_test "Test implies false" "x -> y" [ "x true"; "y false" ] false;
         (* BICONDITIONAL ( <-> ) *)
         make_test "Test biconditional true 1" "x <-> y" [ "x true"; "y true" ]
           true;
         make_test "Test biconditional true 2" "x <-> y"
           [ "x false"; "y false" ] true;
         make_test "Test biconditional false 1" "x <-> y"
           [ "x true"; "y false" ] false;
         make_test "Test biconditional false 2" "x <-> y"
           [ "x false"; "y true" ] false;
         (* Complex Prop Tests *)
         make_test "Test complex expression 1" "(x ^ y) -> z"
           [ "x true"; "y true"; "z true" ]
           true;
         make_test "Test complex expression 2" "(x ^ y) -> z"
           [ "x true"; "y true"; "z false" ]
           false;
         make_test "Test complex expression 3" "(x ^ y) -> z"
           [ "x false"; "y true"; "z false" ]
           true;
         make_test "Test multiple operators" "x -> (y -> z)"
           [ "x true"; "y true"; "z true" ]
           true;
         make_test "Test not and" "~(x ^ y)" [ "x true"; "y true" ] false;
         make_test "Test not or" "~(x v y)" [ "x false"; "y false" ] true;
       ]

let _ = run_test_tt_main suite
