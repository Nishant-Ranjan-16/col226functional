open Secd

(* let%test "test1" = (test_f2 3 = 9);; *)

let fuel = int_of_float (10.0**4.0);;

let identopcode = compile (Lam("x", V("x")));;

let%test "identity_compile" =  (identopcode = [MkCLOS("x", [LOOKUP("x"); RET])]);;

let ident_eval = eval [] [] identopcode [] fuel;;

let%test "identity_eval" = (ident_eval = Clos("x", [LOOKUP("x"); RET], []));;

(*Infinite loop*)

let self = Lam ("x", App(V("x"), V("x")));;
let infinite_loop = App (self, self);;
let infinite_opcode = compile infinite_loop;;

let%test "infinite_opcode" = (infinite_opcode = [MkCLOS("x", [LOOKUP("x"); LOOKUP("x"); APP; RET]); MkCLOS("x", [LOOKUP("x"); LOOKUP("x"); APP; RET]); APP]);;
let%test "infinite_loop" = (try let _ = eval [] [] infinite_opcode [] fuel in false with InfiniteLoop -> true | _ -> false);;

(*Church booleans*)

let ch_true = Lam ("x", Lam("y", V ("x")));;
let ch_false = Lam ("x", Lam("y", V ("y")));;

let ch_true_opcode = compile ch_true;;
let ch_false_opcode = compile ch_false;;

let%test "church_true_opcode" = (ch_true_opcode = [MkCLOS("x", [MkCLOS("y", [LOOKUP("x"); RET]);RET])]);;
let%test "church_false_opcode" = (ch_false_opcode = [MkCLOS("x", [MkCLOS("y", [LOOKUP("y"); RET]);RET])]);;

(*EAGER EVALUATION TEST*)

let eag1 = App (App(ch_true, ch_true), infinite_loop);;

let eag1_opcode = compile eag1;;

let%test "eager" = (try let _ = eval [] [] eag1_opcode [] fuel in false with InfiniteLoop -> true | _ -> false);;

(*IF_THEN_ELSE*)

let if_else_exp = Lam("b", Lam("x", Lam("y", App(App(V "b", V "x"), V "y"))));;

let test_if_else = compile (App((App (App (if_else_exp, ch_true), self)), ch_true));;

let%test "if_else" = (eval [] [] test_if_else [] fuel = eval [] [] (compile self) [] fuel);;

let test_if_else2 = compile (App((App (App (if_else_exp, ch_false), self)), ch_false));;

let%test "if_else2" = (eval [] [] test_if_else2 [] fuel = eval [] [] (compile ch_false) [] fuel);;


