exception Stuck of string;;
exception InfiniteLoop;;

type variable = string

type lamexp = V of string | Lam of string*lamexp | App of lamexp*lamexp;;

type opcode = LOOKUP of string | APP | MkCLOS of string*(opcode list) | RET;;

type closure = Clos of variable*(opcode list)*( (string*closure) list);; 

let rec find_var x gamma= match gamma with
    | [] -> None
    | (s, cl)::t -> if s=x then Some cl else find_var x t;;


let rec rem x gamma = match gamma with
    [] -> []
    | (s, cl)::t -> if s=x then t else (s,cl)::(rem x t)
;;


let augment x cl gamma = let new_table = rem x gamma in (x,cl)::new_table;;


let rec compile (e:lamexp) : opcode list = match e with 
    |V x -> [LOOKUP (x)]
    |Lam (x, e1) -> [MkCLOS (x, compile(e1)@[RET])]
    |App (e1, e2) -> (compile e1)@(compile e2)@[APP]
;;

(* type secd_clos = SClos of string*(opcode list)*((string*secd_clos) list) | VSClos of string*((string*secd_clos) list);; *)

let rec eval s e c d fuel = if fuel=(-1) then raise InfiniteLoop else match (s,e,c,d) with
(s, gamma, LOOKUP(x)::c_new, d) -> (
    let a = find_var x gamma in match a with 
        |None -> raise (Stuck ("Don't know the value of "^x))
        |Some(v) -> eval (v::s) gamma c_new d (fuel-1)
) 

|(s, gamma, MkCLOS(x, c_new)::c_save, d) -> (
        let new_clos = Clos (x, c_new, gamma) in 
        eval (new_clos::s) gamma c_save d (fuel-1)
)

|(a::Clos(x,c_new, gamma_saved)::s_save, gamma, APP::c_save, d) -> (
        let new_gamma = augment x a gamma_saved in let new_d = (s_save, gamma, c_save)::d in eval [] new_gamma c_new new_d (fuel-1) 
)

|(a::_, _ , RET::_, (s, gamma, c_prime_prime)::d) -> eval (a::s) gamma c_prime_prime d (fuel-1)

|(a::_, _, [], _) -> a 

| _ -> raise (Stuck ("Don't know how to evaluate"))
;; 
