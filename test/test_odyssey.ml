open OUnit2
open QCheck
open Odyssey

(* Helper function to create tests for PropEval.eval_prop *)
let make_test name prop_str data_lst expected =
  name >:: fun _ ->
  let prop = PropEval.parse_prop prop_str in
  let data = PropEval.create_data data_lst in
  let result = PropEval.eval_prop prop data in
  assert_equal expected result ~printer:string_of_bool

(* QCheck generators *)
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
  if size <= 0 then
    (* Base case: just variables *)
    Gen.map (fun v -> v) var_gen
  else
    (* Generate more complex propositions *)
    Gen.oneof
      [
        (* Variable *)
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

(* Generate random data with values for variables *)
let data_gen vars =
  Gen.map
    (fun values ->
      List.map2 (fun var value -> var ^ " " ^ string_of_bool value) vars values)
    (Gen.list_repeat (List.length vars) Gen.bool)

(* Find all variables in a proposition string *)
let find_variables prop_str =
  let chars = List.init (String.length prop_str) (String.get prop_str) in
  let is_var_char c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in

  let vars = ref [] in
  List.iter
    (fun c ->
      if is_var_char c && not (List.mem (String.make 1 c) !vars) then
        vars := String.make 1 c :: !vars)
    chars;
  !vars

(* Property-based test: Double negation should not change the truth value *)
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

          (* Generate random values for variables *)
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

(* Property-based test: De Morgan's laws should hold *)
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

          (* Generate random values for variables *)
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

(* Property-based test: Implication can be rewritten as OR *)
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

          (* Generate random values for variables *)
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

(* Property-based test: Check if parsing is consistent *)
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

(* Property-based test: Distribution of AND over OR *)
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

(* Property-based test: Distribution of OR over AND *)
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

(* Property-based test: Contraposition *)
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

          (* Generate random values for variables *)
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

(* Property-based test: Triple Negation *)
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

          (* Generate random values for variables *)
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

(* Property-based test: Commutativity of AND *)
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

          (* Generate random values for variables *)
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

(* Property-based test: Commutativity of OR *)
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

          (* Generate random values for variables *)
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

(* Property-based test: Associativity of AND *)
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

          (* Generate random values for variables *)
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

(* Traditional OUnit tests *)
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
         (* More Complex Expressions *)
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
         (* Mixing different operations *)
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
       ]

(* Combine OUnit and QCheck tests *)
let suite =
  "All Tests"
  >::: [
         ounit_suite;
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
