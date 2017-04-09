
module HW4
(*Ryan Chalmers 260581055*)
type id = string

type term =
  | Var of id
  | Const of int 
  | Term of id * term list

(* invariant for substitutions: *)
(* no id on a lhs occurs in any term earlier in the list *)
type substitution = (id * term) list


(*Checks each a Term for the variable. If its a const return false, if the var matches return true
else return false, if its a term skip and check the next term check the first first term in the term
list and also check a new Term created using the rest of the term list*)
let rec occurs (x : id) (t : term) : bool = 
    match t with
    | Term(_,[]) -> false
    | Const c -> false
    | Var(v) -> x=v 
    | Term(id, y::ys) -> 
      occurs x (Term(id, ys)) || occurs x y
    

(* substitute term s for all occurrences of variable x in term t *)
(*If we are looking at a constant just return the constant. If we are looking at a variable,
check and see if it is the correct variable, if so replacer it if not leavre it. If we are looking at
a term skip it leave the term*)
let rec subst (s : term) (x : id) (t : term) : term = 
  match t with 
  | Const c -> (Const)c
  | Term(id, x) -> Term(id, x) 
  | v ->  if s=v then s
                else v
  
    


(* apply a substitution right to left; use foldBack *)
let apply (s : substitution) (t : term) : term = 
  s |> List.fold(fun t2 (x , t1)  -> subst t1 x t2 ) t

(* unify one pair *)
let rec unify (s : term) (t : term) : substitution = failwith "Not implemented"

(* unify a list of pairs *)
and unify_list (s : (term * term) list) : substitution = failwith "Not implemented"


(*
Examples
> let t1 = Term("f",[Var "x";Var "y"; Term("h",[Var "x"])]);;
val t1 : term = Term ("f",[Var "x"; Var "y"; Term ("h",[Var "x"])])
> let t2 = Term("f", [Term("g",[Var "z"]); Term("h",[Var "x"]); Var "y"]);;
val t2 : term =
  Term ("f",[Term ("g",[Var "z"]); Term ("h",[Var "x"]); Var "y"])
> let t3 = Term("f", [Var "x"; Var "y"; Term("g", [Var "u"])]);;
val t3 : term = Term ("f",[Var "x"; Var "y"; Term ("g",[Var "u"])])
> unify t1 t2;;
val it : substitution =
  [("x", Term ("g",[Var "z"])); ("y", Term ("h",[Var "x"]))]
> let t4 = Term("f", [Var "x"; Term("h", [Var "z"]); Var "x"]);;
val t4 : term = Term ("f",[Var "x"; Term ("h",[Var "z"]); Var "x"])
>  let t5 = Term("f", [Term("k", [Var "y"]); Var "y"; Var "x"]);;
val t5 : term = Term ("f",[Term ("k",[Var "y"]); Var "y"; Var "x"])
> unify t4 t5;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> unify t5 t4;;
val it : substitution =
  [("x", Term ("k",[Term ("h",[Var "z"])])); ("y", Term ("h",[Var "z"]))]
> apply it t4;;
val it : term =
  Term
    ("f",
     [Term ("k",[Term ("h",[Var "z"])]); Term ("h",[Var "z"]);
      Term ("k",[Term ("h",[Var "z"])])])
> let t6 = Term("f", [Const 2; Var "x"; Const 3]);;
val t6 : term = Term ("f",[Const 2; Var "x"; Const 3])
> let t7 = Term("f", [Const 2; Const 3; Var "y"]);;
val t7 : term = Term ("f",[Const 2; Const 3; Var "y"])
> unify t6 t7;;
val it : substitution = [("x", Const 3); ("y", Const 3)]
> apply it t7;;
val it : term = Term ("f",[Const 2; Const 3; Const 3])
> unify t1 t7;;
System.Exception: not unifiable: term constant clash
....... junk removed .............
Stopped due to error
*)