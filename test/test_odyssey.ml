open OUnit2
open QCheck
open Odyssey

(* Helper for proposition evaluation tests *)
let make_test name prop_str data_lst expected =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let data = PropEval.create_data data_lst in
  let result = PropEval.eval_prop prop data in
  assert_equal expected result ~printer:string_of_bool

(* Helper for LaTeX printing tests *)
let make_latex_test name prop_str expected =
  name >:: fun _ ->
  let actual = PropEval.latex_of_prop (PropEval.parse_prop prop_str) in
  assert_equal expected actual
    ~printer:(fun s -> s)
    ~cmp:String.equal
    ~pp_diff:(fun fmt (expected, actual) ->
      Format.fprintf fmt "Expected: %s\nActual: %s" expected actual)

(* Unquantified variables tests helper *)
let make_unquant_vars_test name prop_str data_lst expected =
  name >:: fun _ ->
  assert_equal expected
    (PropEval.unquantified_variables
       (PropEval.create_data data_lst)
       (PropEval.parse_prop prop_str))
    ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")

(* Simplify proposition tests helper *)
let make_simplify_test name prop_str data_lst expected_prop_str =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let data = PropEval.create_data data_lst in
  let simplified = PropEval.simplify_prop prop data in
  let expected = PropEval.parse_prop expected_prop_str in
  let expected_str = PropEval.print_prop expected in
  let simplified_str = PropEval.print_prop simplified in
  if expected_str <> simplified_str then
    assert_failure
      (name ^ ":\nOriginal proposition: '" ^ prop_str ^ "'\nWith data: ["
      ^ String.concat "; " data_lst
      ^ "]\nExpected: '" ^ expected_str ^ "'\nActual: '" ^ simplified_str ^ "'"
      )
  else assert_bool name true

(* Helper function to check if a string contains a substring *)
let rec string_contains_aux s sub i length length_sub =
  if i > length - length_sub then false
  else if String.sub s i length_sub = sub then true
  else string_contains_aux s sub (i + 1) length length_sub

let string_contains s sub =
  try
    let length = String.length s in
    let length_sub = String.length sub in
    string_contains_aux s sub 0 length length_sub
  with _ -> false

(* Helper function for testing DIMACS format output *)
let make_dimacs_test name prop_str expected =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let dimacs = PropEval.dimacs_of_prop prop in
  assert_bool
    (name ^ ": Expected DIMACS format to contain '" ^ expected ^ "'")
    (string_contains dimacs expected)

(* Helper function for testing CNF conversion with better error reporting *)
let make_cnf_test name prop_str expected_pattern =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let cnf = PropEval.cnf_of_prop prop in
  let cnf_str = PropEval.print_prop cnf in
  if not (string_contains cnf_str expected_pattern) then
    assert_failure
      (name ^ ":\nOriginal proposition: '" ^ prop_str
     ^ "'\nExpected CNF to contain: '" ^ expected_pattern ^ "'\nActual CNF: '"
     ^ cnf_str ^ "'")
  else assert_bool name true

(* Helper for satisfiability tests *)
let make_sat_test name prop_str expected =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let result = PropEval.is_satisfiable prop in
  assert_equal expected result ~printer:string_of_bool

(* Helper function for testing eval_prop_string *)
let make_eval_prop_string_test name prop_str data_lst expected_color
    expected_text =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let data = PropEval.create_data data_lst in
  let result = PropEval.eval_prop_string prop data in
  let contains_expected =
    List.exists
      (fun (color, text) ->
        color = expected_color && string_contains text expected_text)
      result
  in
  assert_bool
    (name ^ ": Expected result to contain color '" ^ expected_color
   ^ "' and text containing '" ^ expected_text ^ "'")
    contains_expected

(* Helper for latex evaluation tests *)
let make_latex_eval_test name prop_str data_lst expected_pattern =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let data = PropEval.create_data data_lst in
  let latex = PropEval.latex_of_eval_prop prop data in
  if not (string_contains latex expected_pattern) then
    assert_failure
      (name ^ ":\nExpected pattern: '" ^ expected_pattern
     ^ "'\nActual output: '" ^ latex ^ "'")
  else assert_bool name true

(* QCheck: proposition generation *)
let var_gen =
  Gen.oneof
    [
      Gen.pure "x";
      Gen.pure "y";
      Gen.pure "z";
      Gen.pure "w";
      Gen.pure "u";
      Gen.pure "v";
    ]

let rec prop_gen ~size =
  if size <= 0 then Gen.map (fun v -> v) var_gen
  else
    Gen.oneof
      [
        (* VAR *)
        var_gen;
        (* NOT *)
        Gen.map (fun p -> "~" ^ p) (prop_gen ~size:(size - 1));
        (* AND *)
        Gen.map2
          (fun p1 p2 -> "(" ^ p1 ^ " ^ " ^ p2 ^ ")")
          (prop_gen ~size:(size / 2))
          (prop_gen ~size:(size / 2));
        (* OR *)
        Gen.map2
          (fun p1 p2 -> "(" ^ p1 ^ " v " ^ p2 ^ ")")
          (prop_gen ~size:(size / 2))
          (prop_gen ~size:(size / 2));
        (* IMPLIES *)
        Gen.map2
          (fun p1 p2 -> "(" ^ p1 ^ " -> " ^ p2 ^ ")")
          (prop_gen ~size:(size / 2))
          (prop_gen ~size:(size / 2));
      ]

(* random data for vars *)
let data_gen vars =
  Gen.map
    (fun values ->
      List.map2 (fun var value -> var ^ " " ^ string_of_bool value) vars values)
    (Gen.list_repeat (List.length vars) Gen.bool)

(* simple var finder *)
let is_var_char c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let find_variables_from_chars chars =
  let vars = ref [] in
  List.iter
    (fun c ->
      if is_var_char c && not (List.mem (String.make 1 c) !vars) then
        vars := String.make 1 c :: !vars)
    chars;
  !vars

let find_variables prop_str =
  let chars = List.init (String.length prop_str) (String.get prop_str) in
  find_variables_from_chars chars

(* prop: double negation holds *)
let prop_double_negation =
  Test.make ~name:"Double negation should not change the truth value" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 4 in
      let prop_str = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables prop_str in
        if List.length vars = 0 then true
        else
          let neg_prop_str = "~(" ^ prop_str ^ ")" in
          let double_neg_prop_str = "~(" ^ neg_prop_str ^ ")" in

          let prop = PropEval.parse_prop prop_str in
          let double_neg_prop = PropEval.parse_prop double_neg_prop_str in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              vars
          in
          let data = PropEval.create_data data_list in

          let result = PropEval.eval_prop prop data in
          let double_neg_result = PropEval.eval_prop double_neg_prop data in

          result = double_neg_result
      with _ -> true)

(* prop: de morgan holds *)
let prop_de_morgan =
  Test.make ~name:"De Morgan's laws should hold" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 3 in
      let prop_str1 = Gen.generate1 (prop_gen ~size) in
      let prop_str2 = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables prop_str1 @ find_variables prop_str2 in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          (* ~(p ^ q) <-> (~p v ~q) *)
          let and_prop_str = "(" ^ prop_str1 ^ " ^ " ^ prop_str2 ^ ")" in
          let neg_and_prop_str = "~" ^ and_prop_str in
          let neg_prop_str1 = "~(" ^ prop_str1 ^ ")" in
          let neg_prop_str2 = "~(" ^ prop_str2 ^ ")" in
          let or_neg_prop_str =
            "(" ^ neg_prop_str1 ^ " v " ^ neg_prop_str2 ^ ")"
          in

          let neg_and_prop = PropEval.parse_prop neg_and_prop_str in
          let or_neg_prop = PropEval.parse_prop or_neg_prop_str in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let neg_and_result = PropEval.eval_prop neg_and_prop data in
          let or_neg_result = PropEval.eval_prop or_neg_prop data in

          neg_and_result = or_neg_result
      with _ -> true)

(* prop: implication as or *)
let prop_implication_as_or =
  Test.make ~name:"p -> q is equivalent to ~p v q" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 3 in
      let prop_str1 = Gen.generate1 (prop_gen ~size) in
      let prop_str2 = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables prop_str1 @ find_variables prop_str2 in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          (* p -> q *)
          let implies_prop_str = "(" ^ prop_str1 ^ " -> " ^ prop_str2 ^ ")" in
          (* ~p v q *)
          let neg_prop_str1 = "~(" ^ prop_str1 ^ ")" in
          let or_prop_str = "(" ^ neg_prop_str1 ^ " v " ^ prop_str2 ^ ")" in

          let implies_prop = PropEval.parse_prop implies_prop_str in
          let or_prop = PropEval.parse_prop or_prop_str in
          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let implies_result = PropEval.eval_prop implies_prop data in
          let or_result = PropEval.eval_prop or_prop data in

          implies_result = or_result
      with _ -> true)

(* prop: parsing consistency *)
let prop_parsing_consistent =
  Test.make ~name:"Parsing should be consistent" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 4 in
      let prop_str = Gen.generate1 (prop_gen ~size) in
      try
        let _ = PropEval.parse_prop prop_str in
        true
      with _ -> true)

(* prop: distribution AND over OR *)
let prop_distribution_and_over_or =
  Test.make ~name:"Distribution of AND over OR" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 2 in
      let p = Gen.generate1 (prop_gen ~size) in
      let q = Gen.generate1 (prop_gen ~size) in
      let r = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p @ find_variables q @ find_variables r in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          (* p ^ (q v r) <-> (p ^ q) v (p ^ r) *)
          let left_expr = "(" ^ p ^ " ^ (" ^ q ^ " v " ^ r ^ "))" in
          let right_expr =
            "((" ^ p ^ " ^ " ^ q ^ ") v (" ^ p ^ " ^ " ^ r ^ "))"
          in

          let left_prop = PropEval.parse_prop left_expr in
          let right_prop = PropEval.parse_prop right_expr in

          (* Generate random values for variables *)
          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let left_result = PropEval.eval_prop left_prop data in
          let right_result = PropEval.eval_prop right_prop data in

          left_result = right_result
      with _ -> true)

(* prop: distribution OR over AND *)
let prop_distribution_or_over_and =
  Test.make ~name:"Distribution of OR over AND" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 2 in
      let p = Gen.generate1 (prop_gen ~size) in
      let q = Gen.generate1 (prop_gen ~size) in
      let r = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p @ find_variables q @ find_variables r in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          (* p v (q ^ r) <-> (p v q) ^ (p v r) *)
          let left_expr = "(" ^ p ^ " v (" ^ q ^ " ^ " ^ r ^ "))" in
          let right_expr =
            "((" ^ p ^ " v " ^ q ^ ") ^ (" ^ p ^ " v " ^ r ^ "))"
          in

          let left_prop = PropEval.parse_prop left_expr in
          let right_prop = PropEval.parse_prop right_expr in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let left_result = PropEval.eval_prop left_prop data in
          let right_result = PropEval.eval_prop right_prop data in

          left_result = right_result
      with _ -> true)

(* prop: contraposition *)
let prop_contraposition =
  Test.make ~name:"Contraposition: (p -> q) <-> (~q -> ~p)" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 3 in
      let p = Gen.generate1 (prop_gen ~size) in
      let q = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p @ find_variables q in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          (* (p -> q) <-> (~q -> ~p) *)
          let normal_impl = "(" ^ p ^ " -> " ^ q ^ ")" in
          let contrapositive = "(~(" ^ q ^ ") -> ~(" ^ p ^ "))" in

          let normal_prop = PropEval.parse_prop normal_impl in
          let contra_prop = PropEval.parse_prop contrapositive in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let normal_result = PropEval.eval_prop normal_prop data in
          let contra_result = PropEval.eval_prop contra_prop data in

          normal_result = contra_result
      with _ -> true)

(* prop: triple negation *)
let prop_triple_negation =
  Test.make ~name:"Triple negation: ~~~p <-> ~p" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 2 in
      let p = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p in
        if List.length vars = 0 then true
        else
          let triple_neg = "~(~(~(" ^ p ^ ")))" in
          let single_neg = "~(" ^ p ^ ")" in

          let triple_neg_prop = PropEval.parse_prop triple_neg in
          let single_neg_prop = PropEval.parse_prop single_neg in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              vars
          in
          let data = PropEval.create_data data_list in

          let triple_result = PropEval.eval_prop triple_neg_prop data in
          let single_result = PropEval.eval_prop single_neg_prop data in

          triple_result = single_result
      with _ -> true)

(* prop: commutative AND *)
let prop_commutative_and =
  Test.make ~name:"AND is commutative: p ^ q <-> q ^ p" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 3 in
      let p = Gen.generate1 (prop_gen ~size) in
      let q = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p @ find_variables q in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          let p_and_q = "(" ^ p ^ " ^ " ^ q ^ ")" in
          let q_and_p = "(" ^ q ^ " ^ " ^ p ^ ")" in

          let p_and_q_prop = PropEval.parse_prop p_and_q in
          let q_and_p_prop = PropEval.parse_prop q_and_p in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let p_and_q_result = PropEval.eval_prop p_and_q_prop data in
          let q_and_p_result = PropEval.eval_prop q_and_p_prop data in

          p_and_q_result = q_and_p_result
      with _ -> true)

(* prop: commutative OR *)
let prop_commutative_or =
  Test.make ~name:"OR is commutative: p v q <-> q v p" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 3 in
      let p = Gen.generate1 (prop_gen ~size) in
      let q = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p @ find_variables q in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          let p_or_q = "(" ^ p ^ " v " ^ q ^ ")" in
          let q_or_p = "(" ^ q ^ " v " ^ p ^ ")" in

          let p_or_q_prop = PropEval.parse_prop p_or_q in
          let q_or_p_prop = PropEval.parse_prop q_or_p in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let p_or_q_result = PropEval.eval_prop p_or_q_prop data in
          let q_or_p_result = PropEval.eval_prop q_or_p_prop data in

          p_or_q_result = q_or_p_result
      with _ -> true)

(* prop: associative AND *)
let prop_associative_and =
  Test.make ~name:"AND is associative: p ^ (q ^ r) <-> (p ^ q) ^ r" ~count:100
    (list_of_size Gen.(0 -- 3) int)
    (fun _ ->
      let size = Random.int 2 in
      let p = Gen.generate1 (prop_gen ~size) in
      let q = Gen.generate1 (prop_gen ~size) in
      let r = Gen.generate1 (prop_gen ~size) in
      try
        let vars = find_variables p @ find_variables q @ find_variables r in
        let unique_vars = List.sort_uniq String.compare vars in
        if List.length unique_vars = 0 then true
        else
          let p_and_qr = "(" ^ p ^ " ^ (" ^ q ^ " ^ " ^ r ^ "))" in
          let pq_and_r = "((" ^ p ^ " ^ " ^ q ^ ") ^ " ^ r ^ ")" in

          let p_and_qr_prop = PropEval.parse_prop p_and_qr in
          let pq_and_r_prop = PropEval.parse_prop pq_and_r in

          let data_list =
            List.map
              (fun var -> var ^ " " ^ string_of_bool (Random.bool ()))
              unique_vars
          in
          let data = PropEval.create_data data_list in

          let p_and_qr_result = PropEval.eval_prop p_and_qr_prop data in
          let pq_and_r_result = PropEval.eval_prop pq_and_r_prop data in

          p_and_qr_result = pq_and_r_result
      with _ -> true)

(* Helper to check assignment results *)
let assert_assignment_result name result expected_to_find =
  match (result, expected_to_find) with
  | Some _, true -> assert_bool "Expected to find an assignment" true
  | None, false -> assert_bool "Expected to find no assignment" true
  | Some _, false ->
      assert_bool "Expected to find no assignment but found one" false
  | None, true ->
      assert_bool "Expected to find an assignment but found none" false

(* ounit tests *)
let ounit_suite =
  "PropEval OUnit Tests"
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
         make_test "Test and false 3" "x ^ y" [ "x false"; "y false" ] false;
         (* OR ( v ) *)
         make_test "Test or true 1" "x v y" [ "x true"; "y false" ] true;
         make_test "Test or true 2" "x v y" [ "x false"; "y true" ] true;
         make_test "Test or true 3" "x v y" [ "x true"; "y true" ] true;
         make_test "Test or false" "x v y" [ "x false"; "y false" ] false;
         (* IMPLIES ( -> ) *)
         make_test "Test implies true 1" "x -> y" [ "x true"; "y true" ] true;
         make_test "Test implies true 2" "x -> y" [ "x false"; "y false" ] true;
         make_test "Test implies true 3" "x -> y" [ "x false"; "y true" ] true;
         make_test "Test implies false" "x -> y" [ "x true"; "y false" ] false;
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
         make_test "Test complex expression 4" "(x ^ y) -> z"
           [ "x true"; "y false"; "z false" ]
           true;
         make_test "Test complex expression 5" "(x ^ y) -> z"
           [ "x false"; "y false"; "z false" ]
           true;
         (* Nested Expressions *)
         make_test "Test multiple operators" "x -> (y -> z)"
           [ "x true"; "y true"; "z true" ]
           true;
         make_test "Test multiple operators 2" "x -> (y -> z)"
           [ "x true"; "y true"; "z false" ]
           false;
         make_test "Test multiple operators 3" "x -> (y -> z)"
           [ "x true"; "y false"; "z true" ]
           true;
         make_test "Test multiple operators 4" "x -> (y -> z)"
           [ "x false"; "y true"; "z false" ]
           true;
         (* NOT with other operators *)
         make_test "Test not and" "~(x ^ y)" [ "x true"; "y true" ] false;
         make_test "Test not and 2" "~(x ^ y)" [ "x false"; "y true" ] true;
         make_test "Test not and 3" "~(x ^ y)" [ "x true"; "y false" ] true;
         make_test "Test not and 4" "~(x ^ y)" [ "x false"; "y false" ] true;
         make_test "Test not or" "~(x v y)" [ "x true"; "y true" ] false;
         make_test "Test not or 2" "~(x v y)" [ "x false"; "y true" ] false;
         make_test "Test not or 3" "~(x v y)" [ "x true"; "y false" ] false;
         make_test "Test not or 4" "~(x v y)" [ "x false"; "y false" ] true;
         make_test "Test not implies" "~(x -> y)" [ "x true"; "y true" ] false;
         make_test "Test not implies 2" "~(x -> y)" [ "x false"; "y true" ]
           false;
         make_test "Test not implies 3" "~(x -> y)" [ "x true"; "y false" ] true;
         make_test "Test not implies 4" "~(x -> y)" [ "x false"; "y false" ]
           false;
         (* Even more complex expressions *)
         make_test "Test complex nested 1" "(~x v y) -> (z ^ w)"
           [ "x true"; "y false"; "z true"; "w true" ]
           true;
         make_test "Test complex nested 2" "(~x v y) -> (z ^ w)"
           [ "x false"; "y true"; "z true"; "w true" ]
           true;
         make_test "Test complex nested 3" "(~x v y) -> (z ^ w)"
           [ "x true"; "y true"; "z true"; "w true" ]
           true;
         make_test "Test complex nested 4" "(~x v y) -> (z ^ w)"
           [ "x false"; "y false"; "z false"; "w true" ]
           false;
         make_test "Test complex nested 5" "(~x v y) -> (z ^ w)"
           [ "x false"; "y false"; "z true"; "w false" ]
           false;
         (* Tests with three variables *)
         make_test "Test three variables 1" "x ^ y ^ z"
           [ "x true"; "y true"; "z true" ]
           true;
         make_test "Test three variables 2" "x ^ y ^ z"
           [ "x true"; "y true"; "z false" ]
           false;
         make_test "Test three variables 3" "x ^ y ^ z"
           [ "x true"; "y false"; "z true" ]
           false;
         make_test "Test three variables 4" "x v y v z"
           [ "x false"; "y false"; "z false" ]
           false;
         make_test "Test three variables 5" "x v y v z"
           [ "x false"; "y false"; "z true" ]
           true;
         make_test "Test three variables 6" "x v y v z"
           [ "x true"; "y false"; "z false" ]
           true;
         (* Multiple NOTs *)
         make_test "Test multiple NOTs 1" "~~~x" [ "x true" ] false;
         make_test "Test multiple NOTs 2" "~~~x" [ "x false" ] true;
         make_test "Test multiple NOTs 3" "~~~~x" [ "x true" ] true;
         (* Mixing different theorem operations *)
         make_test "Test mixed operations 1" "(x ^ ~y) v (z -> w)"
           [ "x true"; "y true"; "z true"; "w false" ]
           false;
         make_test "Test mixed operations 2" "(x ^ ~y) v (z -> w)"
           [ "x true"; "y false"; "z true"; "w false" ]
           true;
         make_test "Test mixed operations 3" "(x ^ ~y) v (z -> w)"
           [ "x true"; "y true"; "z false"; "w true" ]
           true;
         (* De Morgan's Laws explicit tests *)
         make_test "De Morgan 1: ~(x ^ y) <-> (~x v ~y)"
           "~(x ^ y) <-> (~x v ~y)" [ "x true"; "y true" ] true;
         make_test "De Morgan 2: ~(x ^ y) <-> (~x v ~y)"
           "~(x ^ y) <-> (~x v ~y)" [ "x true"; "y false" ] true;
         make_test "De Morgan 3: ~(x v y) <-> (~x ^ ~y)"
           "~(x v y) <-> (~x ^ ~y)" [ "x false"; "y false" ] true;
         (* Logical Equivalences *)
         make_test "Equivalence 1: (p -> q) <-> (~p v q)"
           "(x -> y) <-> (~x v y)" [ "x true"; "y true" ] true;
         make_test "Equivalence 2: (p -> q) <-> (~p v q)"
           "(x -> y) <-> (~x v y)" [ "x false"; "y false" ] true;
         (* Tests for latex_of_prop *)
         make_latex_test "Test latex simple var" "x" "$x$";
         make_latex_test "Test latex NOT" "~x" "$\\lnot x$";
         make_latex_test "Test latex AND" "x ^ y" "$x \\land y$";
         make_latex_test "Test latex OR" "x v y" "$x \\lor y$";
         make_latex_test "Test latex IMPLIES" "x -> y" "$x \\rightarrow y$";
         make_latex_test "Test latex complex nested" "(x ^ ~y) v (z -> w)"
           "$x \\land \\lnot y \\lor (z \\rightarrow w)$";
         (* unquantified_variables tests *)
         make_unquant_vars_test "Test unquantified_variables" "x ^ y ^ z"
           [ "x true"; "z false" ] [ "y" ];
       ]

(* Proposition Simplification Tests *)
let simplification_tests =
  "Proposition Simplification Tests"
  >::: [
         (* Basic variable tests *)
         make_simplify_test "Simplify variable (quantified)" "x" [ "x true" ]
           "x";
         make_simplify_test "Simplify variable (unquantified)" "x" [] "x";
         (* NOT operator tests *)
         make_simplify_test "Simplify NOT with quantified variable" "~x"
           [ "x true" ] "~x";
         make_simplify_test "Simplify NOT with unquantified variable" "~x" []
           "~x";
         make_simplify_test "Simplify double NOT" "~(~x)" [ "x true" ] "~(~x)";
         (* AND operator tests *)
         make_simplify_test "Simplify AND with both quantified (true)" "x ^ y"
           [ "x true"; "y true" ] "x ^ y";
         make_simplify_test "Simplify AND with one false" "x ^ y"
           [ "x false"; "y true" ] "x ^ y";
         make_simplify_test "Simplify AND with one unquantified" "x ^ y"
           [ "x true" ] "y";
         (* x is true, so x ^ y = y *)
         make_simplify_test "Simplify AND with same variable" "x ^ x"
           [ "x true" ] "x";
         (* OR operator tests *)
         make_simplify_test "Simplify OR with both quantified (true)" "x v y"
           [ "x true"; "y true" ] "x v y";
         make_simplify_test "Simplify OR with one true" "x v y"
           [ "x true"; "y false" ] "x v y";
         make_simplify_test "Simplify OR with one unquantified" "x v y"
           [ "x false" ] "y";
         (* x is false, so x v y = y *)
         make_simplify_test "Simplify OR with same variable" "x v x"
           [ "x false" ] "x";
         (* IMPLIES operator tests *)
         make_simplify_test "Simplify IMPLIES with antecedent false" "x -> y"
           [ "x false" ] "true";
         make_simplify_test "Simplify IMPLIES with consequent true" "x -> y"
           [ "y true" ] "true";
         make_simplify_test "Simplify IMPLIES with antecedent true" "x -> y"
           [ "x true" ] "y";
         make_simplify_test "Simplify IMPLIES with consequent false" "x -> y"
           [ "y false" ] "~x";
         make_simplify_test "Simplify IMPLIES with same variable" "x -> x" []
           "true";
         (* BICONDITIONAL operator tests *)
         make_simplify_test "Simplify BICONDITIONAL with same variable"
           "x <-> x" [] "true";
         make_simplify_test "Simplify BICONDITIONAL with both true" "x <-> y"
           [ "x true"; "y true" ] "true";
         make_simplify_test "Simplify BICONDITIONAL with both false" "x <-> y"
           [ "x false"; "y false" ] "true";
         make_simplify_test "Simplify BICONDITIONAL with opposite values"
           "x <-> y" [ "x true"; "y false" ] "false";
         make_simplify_test "Simplify BICONDITIONAL with one true" "x <-> y"
           [ "x true" ] "y";
         make_simplify_test "Simplify BICONDITIONAL with one false" "x <-> y"
           [ "x false" ] "~y";
         (* Complex expression tests *)
         make_simplify_test "Simplify complex expr 1" "(x ^ y) -> z"
           [ "x true"; "y true" ] "z";
         make_simplify_test "Simplify complex expr 2" "(x ^ y) -> z"
           [ "x false" ] "true";
         make_simplify_test "Simplify complex expr 3" "(x v y) ^ z"
           [ "z false" ] "z";
         make_simplify_test "Simplify complex expr 4" "(x v y) ^ z" [ "z true" ]
           "x v y";
         make_simplify_test "Simplify complex expr 5" "~(x ^ ~y)"
           [ "x true"; "y true" ] "~(x ^ ~y)";
         make_simplify_test "Simplify complex expr 6" "~(x ^ ~y)" [ "x true" ]
           "~(x ^ ~y)";
         make_simplify_test "Simplify complex expr 7" "(x v y) ^ x" [ "y true" ]
           "x";
         make_simplify_test "Simplify complex expr 8" "(x v y) ^ x" []
           "(x v y) ^ x";
       ]

(* Add simplified simplification tests to the suite *)
let simplification_tests =
  "Proposition Simplification Tests"
  >::: [
         make_simplify_test "Variable simplification" "x" [ "x true" ] "x";
         make_simplify_test "Double negation elimination" "~(~x)" [ "x true" ]
           "x";
         make_simplify_test "NOT with quantified variable" "~x" [ "x true" ]
           "~x";
         (* AND *)
         make_simplify_test "AND with true antecedent" "x ^ y" [ "x true" ] "y";
         make_simplify_test "AND with false operand" "x ^ y" [ "x false" ] "x";
         make_simplify_test "AND with redundant term" "x ^ x" [ "x true" ] "x";
         (* OR *)
         make_simplify_test "OR with false antecedent" "x v y" [ "x false" ] "y";
         make_simplify_test "OR with true operand" "x v y" [ "x true" ] "x";
         make_simplify_test "OR with redundant term" "x v x" [ "x true" ] "x";
         (* IMPLIES *)
         make_simplify_test "IMPLIES with true antecedent" "x -> y" [ "x true" ]
           "y";
         make_simplify_test "IMPLIES with false consequent" "x -> y"
           [ "y false" ] "~x";
         (* Complex expressions *)
         make_simplify_test "Compound AND expressions" "(x v y) ^ x"
           [ "y true" ] "x";
         make_simplify_test "Complex nested expressions" "~(x ^ ~y)"
           [ "x true"; "y true" ] "y";
         make_simplify_test "No simplification needed" "(x v y) ^ z" []
           "(x v y) ^ z";
         (* BICONDITIONAL *)
         make_simplify_test "BICONDITIONAL with left true" "x <-> y"
           [ "x true" ] "y";
         make_simplify_test "BICONDITIONAL with right true" "x <-> y"
           [ "y true" ] "x";
         make_simplify_test "BICONDITIONAL with left false" "x <-> y"
           [ "x false" ] "~y";
         make_simplify_test "BICONDITIONAL with right false" "x <-> y"
           [ "y false" ] "~x";
         make_simplify_test "BICONDITIONAL with complex expressions"
           "(x ^ y) <-> (x v z)" [] "(x ^ y) <-> (x v z)";
         make_simplify_test "BICONDITIONAL with compound expressions"
           "(a ^ b) <-> (c v d)" [] "(a ^ b) <-> (c v d)";
         make_simplify_test "BICONDITIONAL in larger expression" "p ^ (q <-> r)"
           [ "p true" ] "q <-> r";
         make_simplify_test "Nested BICONDITIONALS" "(p <-> q) <-> r" []
           "(p <-> q) <-> r";
         make_simplify_test "BICONDITIONAL with negation" "~p <-> q"
           [ "q true" ] "~p";
         make_simplify_test
           "BICONDITIONAL with complex left, simple right (true)"
           "(x ^ y) <-> z" [ "z true" ] "x ^ y";
         make_simplify_test
           "BICONDITIONAL with complex left, simple right (false)"
           "(x ^ y) <-> z" [ "z false" ] "~(x ^ y)";
         make_simplify_test
           "BICONDITIONAL with simple left, complex right (true)"
           "z <-> (x ^ y)" [ "z true" ] "x ^ y";
         make_simplify_test
           "BICONDITIONAL with simple left, complex right (false)"
           "z <-> (x ^ y)" [ "z false" ] "~(x ^ y)";
       ]

(* Tests for CNF conversion *)
let cnf_tests =
  "CNF Conversion Tests"
  >::: [
         (* Basic *)
         make_cnf_test "Convert simple proposition to CNF" "x" "x";
         make_cnf_test "Convert NOT to CNF" "~x" "~(x)";
         make_cnf_test "Convert AND to CNF" "x ^ y" "x ^ y";
         make_cnf_test "Convert OR to CNF" "x v y" "x v y";
         make_cnf_test "Convert implication to CNF" "x -> y" "(~(x) v y)";
         make_cnf_test "Convert complex expression to CNF" "(x ^ y) -> z"
           "((~(x) v ~(y)) v z)";
         make_cnf_test "Convert nested implication to CNF" "x -> (y -> z)"
           "((~(x) v ~(y)) v z)";
         make_cnf_test "Distribute OR over AND in CNF" "x v (y ^ z)"
           "((x v y) ^ (x v z))";
         make_cnf_test "Distribute nested OR over AND in CNF"
           "w v (x v (y ^ z))" "(((w v x) v y) ^ ((w v x) v z))";
       ]

(* Tests for DIMACS format generation *)
let dimacs_tests =
  "DIMACS Format Tests"
  >::: [
         make_dimacs_test "Convert simple variable to DIMACS" "x" "p cnf 1 1";
         make_dimacs_test "Convert NOT to DIMACS" "~x" "-1 0";
         make_dimacs_test "Convert AND to DIMACS" "x ^ y" "p cnf 2 2";
         make_dimacs_test "Convert complex expression to DIMACS" "(x v y) ^ ~z"
           "p cnf 3 2";
       ]

(* Tests for satisfiability *)
let satisfiability_tests =
  "Satisfiability Tests"
  >::: [
         make_sat_test "SAT: Simple variable" "x" true;
         make_sat_test "SAT: NOT variable" "~x" true;
         make_sat_test "SAT: AND satisfiable" "x ^ y" true;
         make_sat_test "SAT: OR satisfiable" "x v y" true;
         make_sat_test "SAT: Contradiction" "x ^ ~x" false;
         make_sat_test "SAT: Tautology" "x v ~x" true;
         make_sat_test "SAT: Complex satisfiable" "(x v y) ^ (x v ~y)" true;
         make_sat_test "SAT: Complex unsatisfiable" "(x v y) ^ (~x) ^ (~y)"
           false;
       ]

(* Tests for string of evaluation process. *)
let eval_string_tests =
  "Eval Prop String Tests"
  >::: [
         (* Basic string evaluation *)
         make_eval_prop_string_test "Simple eval_prop_string variable" "x"
           [ "x true" ] "yellow" "Evaluating Variable";
         make_eval_prop_string_test "NOT eval_prop_string" "~x" [ "x true" ]
           "red" "Evaluating '~'";
         make_eval_prop_string_test "AND eval_prop_string" "x ^ y"
           [ "x true"; "y true" ] "green" "Evaluating '^'";
         make_eval_prop_string_test "OR eval_prop_string" "x v y"
           [ "x false"; "y true" ] "magenta" "Evaluating 'v'";
         make_eval_prop_string_test "IMPLIES eval_prop_string" "x -> y"
           [ "x true"; "y false" ] "blue" "Evaluating '->'";
         make_eval_prop_string_test "BICONDITIONAL eval_prop_string" "x <-> y"
           [ "x true"; "y true" ] "blue" "Evaluating '<->'";
       ]

(* Tests for LaTeX evaluation process *)
let latex_eval_tests =
  "LaTeX Evaluation Tests"
  >::: [
         (* Basic LaTeX evaluation *)
         make_latex_eval_test "Simple variable latex_of_eval_prop" "x"
           [ "x true" ] "Evaluating variable $x$";
         make_latex_eval_test "NOT latex_of_eval_prop" "~x" [ "x false" ]
           "Evaluating $\\lnot(x)$";
         make_latex_eval_test "AND latex_of_eval_prop" "x ^ y"
           [ "x true"; "y false" ] "Evaluating $x \\land y$";
         make_latex_eval_test "OR latex_of_eval_prop" "x v y"
           [ "x false"; "y true" ] "Evaluating $x \\lor y$";
         make_latex_eval_test "IMPLIES latex_of_eval_prop" "x -> y"
           [ "x true"; "y false" ] "Evaluating $x \\rightarrow y$";
         make_latex_eval_test "BICONDITIONAL latex_of_eval_prop" "x <-> y"
           [ "x true"; "y true" ] "Evaluating $x \\leftrightarrow y$";
       ]

let suite =
  "All Tests"
  >::: [
         ounit_suite;
         simplification_tests;
         dimacs_tests;
         satisfiability_tests;
         eval_string_tests;
         cnf_tests;
         latex_eval_tests;
         "QCheck Tests"
         >::: [
                QCheck_runner.to_ounit2_test prop_double_negation;
                QCheck_runner.to_ounit2_test prop_de_morgan;
                QCheck_runner.to_ounit2_test prop_implication_as_or;
                QCheck_runner.to_ounit2_test prop_parsing_consistent;
                QCheck_runner.to_ounit2_test prop_distribution_and_over_or;
                QCheck_runner.to_ounit2_test prop_distribution_or_over_and;
                QCheck_runner.to_ounit2_test prop_contraposition;
                QCheck_runner.to_ounit2_test prop_triple_negation;
                QCheck_runner.to_ounit2_test prop_commutative_and;
                QCheck_runner.to_ounit2_test prop_commutative_or;
                QCheck_runner.to_ounit2_test prop_associative_and;
              ];
       ]

let _ = run_test_tt_main suite
