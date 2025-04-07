open Krivine;;

let%test "test1" = (Krivine.test_f 2 = 4);;
let fuel = int_of_float (10.0**8.0);;

let%test "identity" = (Krivine.k_solve (Clos (Lamda("x", Var("x")), [])) [] fuel = VClos (Lamda("x", Var("x")), []));;

let self = Lamda ("x", App(Var("x"), Var("x")));;

let infinite_loop = App (self, self);;

let%test "inf_loop" = (try let _ = Krivine.k_solve (Clos (infinite_loop, [])) [] fuel in false with Infinite_loop -> true | _ -> false);;

let ch_tru = Lamda ("x", Lamda("y", Var ("x")));;

let %test "church_tru" = (k_solve (Clos (ch_tru, [])) [] fuel = VClos (ch_tru, []));;

(*Lazy evaluation test*)

let laz1 = App (App(ch_tru, ch_tru), infinite_loop);;

let%test "lazy_check" = (let a = k_solve (Clos (laz1, [])) [] fuel in
    match a with VClos (b, _) -> b=ch_tru 
);;
