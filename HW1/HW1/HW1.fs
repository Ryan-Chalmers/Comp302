namespace HW1

(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Alfred E. Neumann, Id Number: 17294104 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)
module hw1_sol = 

(* Question 1 *) (* Do not edit this line. *)
//COMPLETE
let rec sumlist l = 
    match l with
    |[]-> (0.0)
    |x::xs-> x + sumlist(xs)

let rec pairlists twolists =
    match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs,ys)


let rec prodList pairlist = 
    match pairlist with
    | ([]) -> []
    | (x::xs) ->
        let (a,b) = x 
        (a*b)::prodList(xs)

let w_mean weights data = 
    (sumlist(prodList(pairlists(weights, data))) / sumlist(weights))

  
(* Question 2. *)  (* Do not edit this line. *)
//COMPLETE
let rec memberof pair = 
    match pair with
    | (x,[])-> false
    //if(x, y::ys) has x=y then we get true else try with (x,ys)
    | (x, y::ys)-> (x = y) || memberof(x, ys)
    

let rec remove(item, lst) = 
    match lst with
    | [] -> lst
    | x::xs when x = item -> remove(item, xs)
    | x::xs -> x::remove(item, xs)

(* Question 3. *)  (* Do not edit this line. *)
//COMPLETE
let findMax l = 
  let rec helper(l,m) = 
    match l with 
    | [] -> m
    | (x::xs) when x>m -> helper(xs, x)
    | (x::xs) when x<=m -> helper(xs, m)

  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)

(* Question 4. *)  (* Do not edit this line. *)
  //ERROR
let rec selsort l = 
    match l with
    | [] -> []
    | (x::xs) when findMax(xs) > x -> findMax(xs)::selsort(x::remove(findMax(xs), xs))
    | (x::xs) when findMax(xs) < x -> x::selsort(xs)
    | (x::xs) when findMax(xs) = x -> selsort(xs)

(* Question 5. *)  (* Do not edit this line. *)
//COMPLETE works but contains duplicates
let rec check(item,list) = 
    match list with 
    | [] -> []
    | x::xs when x = item -> x::check(item,[])
    | x::xs -> check(item, xs)

let rec common twolists = 
 match twolists with 
 | ([], []) -> []
 | (x::xs, y) -> check(x,y)@common(xs, y) 
 | ([], y) -> []



(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)
let rec length l =
    match l with
    |[]-> 0
    |x::xs -> length(xs) + 1
   
let iseven length =
    if(length % 2 = 0) then true
    else false

let rec split l = 
    match l with 
    |[] -> []
    |x::xs -> failwith "not implemented"
    

let rec merge twolists = failwith "Not implemented"

let rec mergesort l = 
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns -> failwith "Not implemented"


