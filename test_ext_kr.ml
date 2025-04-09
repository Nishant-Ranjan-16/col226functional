open K_ext;;

let%test "test1" = (K_ext.test_f 2 = 4);;
let fuel = int_of_float (10.0**8.0);;

let%test "identity" = (K_ext.k_solve (Clos (Lam("x", V("x")), [])) [] fuel = VClos (Lam("x", V("x")), []));;

let self = Lam ("x", App(V("x"), V("x")));;

let infinite_loop = App (self, self);;

let%test "inf_loop" = (try let _ = K_ext.k_solve (Clos (infinite_loop, [])) [] fuel in false with Infinite_loop -> true | _ -> false);;

let ch_true = Lam ("x", Lam("y", V ("x")));;
let ch_false = Lam ("x", Lam("y", V ("y")));;

let %test "church_true" = (k_solve (Clos (ch_true, [])) [] fuel = VClos (ch_true, []));;

(*Lazy evaluation test*)

let laz1 = App (App(ch_true, ch_true), infinite_loop);;

let%test "lazy_check" = (let a = k_solve (Clos (laz1, [])) [] fuel in
    match a with VClos (b, _) -> b=ch_true 
);; 

let if_else_exp = Lam("b", Lam("x", Lam("y", App(App(V "b", V "x"), V "y"))));;

let app = (App((App (App (if_else_exp, ch_true), self)), infinite_loop));;

let%test "if_else" = (let a = k_solve (Clos (app, [])) [] fuel in
    match a with VClos (b, _) -> b=self 
);;

let app2 = (App((App (App (if_else_exp, ch_false), self)), infinite_loop));;

let%test "if_else2" = (try let _ = k_solve (Clos (app2, [])) [] fuel in false with Infinite_loop -> true | _ -> false);;

(*arithmetic functions*)

let sub = Lam ("x", Lam("y", Sub(V("x"), V("y"))));;
let new_sub = App(App(sub, V"y"), V"x");;

let clos_now = Clos(new_sub, [("x", Clos(Int 5, [])); ("y", Clos(Int 10, []))]);;

let%test "sub" = (k_solve clos_now [] fuel = VClos (Int (5), []));;

