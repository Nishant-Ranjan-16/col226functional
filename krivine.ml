let test_f x = x*2;;
type variable = string
type lamexp = V of string | Lam of string*lamexp | App of lamexp*lamexp;;

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
;;


let rec unload (clos: closure) = match clos with 
    Clos (V (x), gamma) -> (let ans = find_var x gamma in match ans with 
            |None -> V(x)
            |Some (t) -> unload t
    )
    | Clos (Lam(x, e), gamma) -> (let new_gamma = rem x gamma in Lam(x, unload (Clos(e, new_gamma))))
    | Clos (App(e1, e2), gamma) -> App (unload (Clos(e1,gamma)) , unload (Clos(e2,gamma)))

;;


