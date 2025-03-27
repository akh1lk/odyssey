let prop = Odyssey.PropEval.parse_prop "( ~ x ^ y ) -> z" in

(* Create data assigning truth values to some variables. In this example, "x"
   and "z" are assigned, while "y" is unquantified. *)
let data = Odyssey.PropEval.create_data [ "x true"; "y true"; "z true" ] in

Odyssey.PropEval.eval_prop prop data
