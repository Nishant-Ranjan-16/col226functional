let test_f x = x*2;;
type variable = string
type lamexp = V of string | Lam of string*lamexp | App of lamexp*lamexp| Int of int | Add of lamexp*lamexp |
    Sub of lamexp*lamexp | Mul of lamexp*lamexp | Div of lamexp*lamexp | Mod of lamexp*lamexp | Neg of lamexp
    | Eq of lamexp*lamexp | Leq of lamexp*lamexp | Geq of lamexp*lamexp | And of lamexp*lamexp | Or of lamexp*lamexp
    |Bool  of bool  | Not of lamexp
;;
(*I will be using a simple table (a list of variables*closures)*)

(*for Krivine machine, a closure is simply an expression*)
;;

(*I will be using a simple table (a list of variables*closures)*)

(*for Krivine machine, a closure is simply an expression*)

type closure = Clos of lamexp*((string*closure) list);;

type answer = VClos of lamexp*((string*closure) list);;

(*you cannot bind x to a non answer*)


type table = (string*closure) list;;
(*We have to device this to get from a closure to an answer*)

exception Stuck of string;;
exception Infinite_loop;;

let rec find_var x gamma= match gamma with
    | [] -> None
    | (s, cl)::t -> if s=x then Some cl else find_var x t;;

let rec rem x gamma = match gamma with
    [] -> []
    | (s, cl)::t -> if s=x then t else (s,cl)::(rem x t)
;;

let augment x cl gamma = let new_table = rem x gamma in (x,cl)::new_table;;

let rec k_solve (curr_clos: closure) (stack: closure list) (fuel:int) : answer = 
        if fuel=(-1) then raise (Infinite_loop) else
        match (curr_clos, stack) with 
        |(Clos (V x, gamma), s) -> (let v = find_var x gamma in 
            match v with 
            | Some (cl) -> k_solve cl s (fuel-1)
            | None -> raise (Stuck ("Don't know the value of "^x))
        )

        | (Clos (Lam (x, e), gamma), cl::snew) -> k_solve (Clos (e, augment x cl gamma)) snew (fuel-1)

        | (Clos (App (e1, e2), gamma), s) -> k_solve (Clos (e1, gamma)) ((Clos (e2, gamma))::s) (fuel-1)
        
        | (Clos (Lam (x, cl), gamma), []) -> VClos (Lam (x,cl), gamma)

        | (Clos (Int i, _), _) -> VClos (Int i, [])

        | (Clos (Bool b, _), _) -> VClos (Bool b, [])

        | (Clos (Add (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Int (i1 + i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Sub (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Int (i1 - i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Mul (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Int (i1 * i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Div (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Int (i1 / i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Mod (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Int (i1 mod i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Neg e1, gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) in
            match v1 with 
            | VClos (Int i1, _) -> VClos (Int (-i1), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Eq (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Bool (i1 = i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Leq (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Bool (i1 <= i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Geq (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Int i1, _), VClos (Int i2, _)) -> VClos (Bool (i1 >= i2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (And (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Bool b1, _), VClos (Bool b2, _)) -> VClos (Bool (b1 && b2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Or (e1, e2), gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) and v2 = k_solve (Clos (e2, gamma)) [] (fuel-1) in
            match (v1, v2) with 
            | (VClos (Bool b1, _), VClos (Bool b2, _)) -> VClos (Bool (b1 || b2), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )

        | (Clos (Not e1, gamma), _) -> (let v1 = k_solve (Clos (e1, gamma)) [] (fuel-1) in
            match v1 with 
            | VClos (Bool b1, _) -> VClos (Bool (not b1), [])
            | _ -> raise (Stuck ("Don't know how to evaluate"))
        )


;;

(*Note that this is just a heuristic, in real machine, we will have to mark the top of the stack and not peek below it, we will also 
save the rhs on the top of the stack*)



let rec unload (clos: closure) = match clos with 
    Clos (V (x), gamma) -> (let ans = find_var x gamma in match ans with 
            |None -> V(x)
            |Some (t) -> unload t
    )
    | Clos (Lam(x, e), gamma) -> (let new_gamma = rem x gamma in Lam(x, unload (Clos(e, new_gamma))))
    | Clos (App(e1, e2), gamma) -> App (unload (Clos(e1,gamma)) , unload (Clos(e2,gamma)))
    | Clos (Int i, _) -> Int i
    | Clos (Bool b, _) -> Bool b
    | Clos (Add(e1, e2), gamma) -> Add(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Sub(e1, e2), gamma) -> Sub(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Mul(e1, e2), gamma) -> Mul(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Div(e1, e2), gamma) -> Div(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Mod(e1, e2), gamma) -> Mod(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Neg(e1), gamma) -> Neg(unload (Clos(e1,gamma)))

    | Clos (Eq(e1, e2), gamma) -> Eq(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Leq(e1, e2), gamma) -> Leq(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Geq(e1, e2), gamma) -> Geq(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (And(e1, e2), gamma) -> And(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Or(e1, e2), gamma) -> Or(unload (Clos(e1,gamma)), unload (Clos(e2, gamma)))

    | Clos (Not(e1), gamma) -> Not(unload (Clos(e1,gamma)))
;;



