let test_f x = x*2;;

type exp = Var of string | Lamda of string*exp | App of exp*exp;;

(*I will be using a simple table (a list of variables*closures)*)

(*for Krivine machine, a closure is simply an expression*)

type closure = Clos of exp*((string*closure) list);;

type answer = VClos of exp*((string*closure) list);;

(*you cannot bind x to a non answer*)


type table = (string*closure) list;;
(*We have to device this to get from a closure to an answer*)

exception Stuck of string;;
exception Infinite_loop;;

let rec find_var (x:string) (gamma: table) : closure option = match gamma with
    | [] -> None
    | (s, cl)::t -> if s=x then Some cl else find_var x t;;

let rec rem x gamma = match gamma with
    [] -> []
    | (s, cl)::t -> if s=x then t else (s,cl)::(rem x t)
;;

let augment (x:string) (cl:closure) (gamma: table) = let new_table = rem x gamma in (x,cl)::new_table;;

let rec k_solve (curr_clos: closure) (stack: closure list) (fuel:int) : answer = 
        if fuel=(-1) then raise (Infinite_loop) else
        match (curr_clos, stack) with 
        |(Clos (Var x, gamma), s) -> (let v = find_var x gamma in 
            match v with 
            | Some (cl) -> k_solve cl s (fuel-1)
            | None -> raise (Stuck ("Don't know the value of "^x))
        )

        | (Clos (Lamda (x, e), gamma), cl::snew) -> k_solve (Clos (e, augment x cl gamma)) snew (fuel-1)

        | (Clos (App (e1, e2), gamma), s) -> k_solve (Clos (e1, gamma)) ((Clos (e2, gamma))::s) (fuel-1)
        
        | (Clos (Lamda (x, cl), gamma), []) -> VClos (Lamda (x,cl), gamma)
;;


