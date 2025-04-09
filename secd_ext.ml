exception Stuck of string;;
exception InfiniteLoop;;

type variable = string

type lamexp = V of string | Lam of string*lamexp | App of lamexp*lamexp| Int of int | Add of lamexp*lamexp |
    Sub of lamexp*lamexp | Mul of lamexp*lamexp | Div of lamexp*lamexp | Mod of lamexp*lamexp | Neg of lamexp
    | Eq of lamexp*lamexp | Leq of lamexp*lamexp | Geq of lamexp*lamexp | And of lamexp*lamexp | Or of lamexp*lamexp
    |Bool  of bool  | Not of lamexp
;;

type opcode = LOOKUP of string | APP | MkCLOS of string*(opcode list) | RET
|ADD | SUB | MUL | DIV | MOD | NEG | EQ | LEQ | GEQ | AND | OR
| NOT | B of bool | I of int
;;

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
    |Int i -> [I i]
    |Bool b -> [B b]
    |Add (e1, e2) -> (compile e1)@(compile e2)@[ADD]
    |Sub (e1, e2) -> (compile e1)@(compile e2)@[SUB]
    |Mul (e1, e2) -> (compile e1)@(compile e2)@[MUL]
    |Div (e1, e2) -> (compile e1)@(compile e2)@[DIV]
    | Mod (e1, e2) -> (compile e1)@(compile e2)@[MOD]
    | Neg e1 -> (compile e1)@[NEG]
    | Eq (e1, e2) -> (compile e1)@(compile e2)@[EQ]
    | Leq (e1, e2) -> (compile e1)@(compile e2)@[LEQ]
    | Geq (e1, e2) -> (compile e1)@(compile e2)@[GEQ]
    | And (e1, e2) -> (compile e1)@(compile e2)@[AND]
    | Or (e1, e2) -> (compile e1)@(compile e2)@[OR]
    | Not e1 -> (compile e1)@[NOT]
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

|(s, e, (I i)::c_, d) -> let ans_clos = Clos("", [I i], []) in eval (ans_clos::s) e c_ d (fuel-1)

|(s, e, (B b)::c_, d) -> let ans_clos = Clos("", [B b], []) in eval (ans_clos::s) e c_ d (fuel-1)

|(a2::a1::s_, e, ADD::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [I (i1+i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to add"))
)

|(a2::a1::s_, e, SUB::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [I (i1-i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to subtract"))
)

|(a2::a1::s_, e, MUL::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [I (i1*i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to multiply"))
)

|(a2::a1::s_, e, DIV::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [I (i1/i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to divide"))
)

|(a2::a1::s_, e, MOD::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [I (i1 mod i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to mod"))
)

|(a1::s_, e, NEG::c_, d) -> (match a1 with 
    |(Clos(_, [I i1], _)) -> let ans_clos = Clos("", [I (-i1)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to negate"))
)

|(a2::a1::s_, e, EQ::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [B (i1=i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to compare"))
)

|(a2::a1::s_, e, LEQ::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [B (i1<=i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to compare"))
)

|(a2::a1::s_, e, GEQ::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [I i1], _), Clos(_, [I i2], _)) -> let ans_clos = Clos("", [B (i1>=i2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to compare"))
)

|(a2::a1::s_, e, AND::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [B b1], _), Clos(_, [B b2], _)) -> let ans_clos = Clos("", [B (b1 && b2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to compare"))
)

|(a2::a1::s_, e, OR::c_, d) -> (match (a1, a2) with 
    |(Clos(_, [B b1], _), Clos(_, [B b2], _)) -> let ans_clos = Clos("", [B (b1 || b2)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to compare"))
)

|(a1::s_, e, NOT::c_, d) -> (match a1 with 
    |(Clos(_, [B b1], _)) -> let ans_clos = Clos("", [B (not b1)], []) in eval (ans_clos::s_) e c_ d (fuel-1)
    | _ -> raise (Stuck ("Don't know how to negate"))
)

|(a::_, _, [], _) -> a 


| _ -> raise (Stuck ("Don't know how to evaluate"))
;; 
