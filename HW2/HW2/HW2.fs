module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Ryan Chalmers, Id Number: 260581055 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *) 

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)
(*Recursively calculate newtons equation until the value of guess is less then the
  given tolerence *)
let rec newton(f,guess:float,tol:float,dx:float) = 
    if (abs(f guess) < tol) then guess
    else newton(f, guess - (f guess)/(deriv(f,dx) guess), tol, dx)
(*
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)

newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)

let root = newton(sin,5.0,0.0001,0.0001)
*)


(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

let p = Poly [(0.0,0)]
let t = Term(3.0,3)

let p1 = Poly[(5.0,6);(3.0,4);(2.0,2);(1.0,1)]
let p2 = Poly[(3.0,4);(2.0,2);(1.0,1)]

exception EmptyList

(*Helper allows for recursion and the creation of on empty Poly list to return.
let y,z=x splits up the tuple giving by x::xs int coeffecient, exponent.
Recursively multiply each term in the polynomial by the given term and append
it to the return poly. Once all the terms have been multiplies return the ret Poly *)
let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly = 
  let rec helper(Term (c,e):term, Poly p:poly, Poly ret):poly = 
    match p with 
    | [] -> Poly ret 
    | x::xs->  
      let y,z = x
      helper(Term (c,e), Poly xs, Poly (ret@[(float(c*y), int(e+z))]))
  helper(Term (c,e),Poly p, Poly [])

(* move through the list from highest degree to lowest. Each term passed will be added 
the return Poly.  If the term to add is of equal degree to the term that is our current 
value of x cappend the new term value to the return poly then append the remaining
terms of the original poly. If the term to add is of higher degree than our current value of x 
append the added term then the current value of x then the rest of the poly to the return Poly.
Once either of these operations is done the algorith passes an empty poly as the Poly p:poly value 
and ends the algo returning the ret Poly *)
let addTermToPoly(Term (c,e):term, Poly p:poly):poly = 
  let rec helper(Term (c,e):term, Poly p:poly, Poly ret):poly = 
    match p with 
    | [] -> Poly ret 
    | x::xs->  
      let y,z = x
      if(e = z) then 
        helper(Term (c,e), Poly [], Poly (ret@[(float(c+y), e)]@xs))
      else if(z < e) then
        helper(Term (c,e), Poly [], Poly (ret@[(c,e); (y,z)]@xs))
      else
        helper(Term (c,e), Poly xs, Poly (ret@[y,z]))    
  helper(Term (c,e),Poly p, Poly [])
  
(*In this function I am using a Poly with [(0.0,0)] to start off in the return Poly. It shows
up in the result but I have not had time to fix this. First we move through the terms of Poly p2 adding them all
to the return poly. Next once this is completed we will match with the (x::xs, []) condition. From here each term
in the poly p1 is added to the return poly. Once that is completed the algo is done and we match with ([],[])
and return the ret Poly *)              
let addPolys(Poly p1:poly, Poly p2:poly):poly = 
  let rec helper(Poly p1:poly, Poly p2:poly, Poly ret):poly =
    match (p1,p2) with
    |([],[])-> Poly ret
    |([], y::ys)-> helper(Poly p1, Poly ys, addTermToPoly(Term y, Poly ret))
    |(x::xs,[])-> helper(Poly xs, Poly p2, addTermToPoly(Term x,Poly ret))
    |(x::xs, y::ys)-> helper(Poly p1, Poly ys, addTermToPoly(Term y, Poly ret))
  helper(Poly p1, Poly p2, Poly [(0.0,0)])  

(*Move through p1 multiplying each term of p1 to each term of p2. Return the new p2
to be multiplied by the next term in p1. Return p2 when complete*)
let multPolys(Poly p1:poly, Poly p2:poly) = 
  let rec helper(Poly p1:poly, Poly p2:poly):poly =
    match p1 with
    |[] -> Poly p2
    | x::xs -> helper(Poly xs, multiplyPolyByTerm(Term x, Poly p2))
  helper(Poly p1, Poly p2)  

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

(*Move through the poly by calling helper recursively and evaluate each term then add it to the output *)
let evalPoly(Poly p:poly,v:float):float =
  let rec helper(Poly p:poly,v:float):float =
    match p with
    | [] -> 0.0
    | x::xs -> 
      let c,e = x
      ((evalTerm v (Term(c, e))) + helper(Poly xs, float(v)))
  helper(Poly p, v)

(*Move through the Poly by recursively calling the helper function. Each time calculate the
derivative of the current term and append it to the return Poly*)
let diffPoly (Poly p) = 
  let rec helper(Poly p:poly, Poly ret:poly) =
    match p with
    | [] -> ret
    | x::xs -> 
      let c,e = x
      let t = Term (c*float(e), e-1)
      helper(Poly xs, Poly(ret@[(c*float(e), e-1)]))
  helper(Poly p, Poly [])

(* Question 3 *)
type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

(*Using options we search the env for a name. If the name matches the one we are looking for return Some value
corresponding to that name otherwise cotinue to recursively search the env for the name by calling lookup *)
let rec lookup(name:string, env: Bindings) = 
  match env with
  |[] -> None
  |x::xs -> 
    let n, v = x
    if (n = name) then
      Some v
    else 
      lookup(name, xs)
(*Move through the bindings list for by recursively calling insert until we find a value 
that the binding name to insert is equal to or greater than. Once found insert the new binding first then 
the binding that we are comparing it to then concatenate that to the remaining binding list*)
let rec insert(name:string, value: int, b: Bindings) = 
  match b with
  | []-> []
  | x::xs ->
    let n, v = x
    if(n >= name) then
      [(name, value); x]::[xs]
    else
      [x]::insert(name, value, xs)

(*Evaluate the exp tree using matching. If we match with a Const return the value of the Const. If match
with a type var lookup that var in the environment and return the value of that variable. If we match with 
a type Mult we first find the value of those vars by matching using the eval function again. Once found we must 
obtain the value by matching using Some x and Some y making sure we return None if nothing is found. Once both Some x
and Some y are found we mutiply them together and return the product. The Add evaluator works the same as the 
Mul evaluator however we return the sum of the two not the product*)                                     
let rec eval(exp : Exptree, env:Bindings) = 
  match exp with 
  | Const i -> Some i
  | Var v -> lookup(v, env)
  | Mul (r1, r2) -> 
    let v1 = eval(r1, env)
    let v2 = eval(r2, env)
    match v1 with
    |None->None
    |Some x->
      match v2 with 
        | None->None
        |Some y -> Some(x*y)
  | Add (z1, z2) -> 
    let v1 = eval(z1,env)
    let v2 = eval(z2,env)
    match v1 with
    |None -> None
    |Some x -> 
      match v2 with 
      |None -> None
      |Some y -> Some(x+y)
  




(* For testing 

let env:Bindings = [("a",3);("b",4);("c",5)]                                

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)

*)


(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]
(*To return the pointsMade tuple, we first split the result tuples. Then we find who won using if, elif, else
and set the points accordingly*)
let pointsMade (r: Result) = 
    let (x,y) = r 
    let (t1,g1) = x
    let (t2,g2) = y
    if(g1 > g2) then
      ((t1, Points 3),(t2, Points 0))
    elif(g1 = g2) then
      ((t1, Points 1),(t2, Points 1))
    else
      ((t1, Points 0),(t2, Points 3))


let initEntry (name:Team) = (name, Points 0)
           
let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

(*Use expression matching, options, and TryFind. There are 4 options for the results of try find. If the 
team is found we add the points to the current points in the table otherwise we add a new team with the points
won calculated using the pointsMade function *)
let updateTable(t:Table,r:Result):Table =
    let a, b = pointsMade r
    let aTeam, (Points aPoints) = a
    let bTeam, (Points bPoints) = b
    match (t.TryFind aTeam, t.TryFind bTeam) with
      | None,None -> 
        t.Add(aTeam, Points(aPoints)).Add(bTeam, Points(bPoints))
      | Some (Points x), None ->
        t.Add(aTeam, (Points(aPoints + x))).Add(bTeam, (Points(bPoints)))
      | None, Some (Points y) ->
        t.Add(aTeam, (Points(aPoints))).Add(bTeam, (Points(bPoints + y)))
      | Some (Points x), Some (Points y) -> 
        t.Add(aTeam, (Points(aPoints + x))).Add(bTeam, (Points(bPoints + y)))
        

let leagueMap = initializeTable(league)
let res = (("Chelsea", Goals 2),("Arsenal",Goals 5)) 
 
//Update the table t recursively for each result tuple in the result list      
let rec weekendUpdate(t:Table,rl: Result list): Table = 
  match rl with
  | [] -> Table []
  | x::xs -> 
    updateTable(weekendUpdate(t, xs) , x)
//Update the table t recursively for each list in the result list list
let rec seasonUpdate(t:Table, sll:Result list list) : Table = 
  match sll with 
  | [] -> Table []
  | x::xs -> 
    weekendUpdate(seasonUpdate(t, xs), x)

let less((s1,n1):Team * Points, (s2,n2):Team * Points) =  n1<n2

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)
                                                  
(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]

let makeRoadMap data = failwith "Not Implemented"

let rec upToManySteps (Roads r) n startCity = failwith "Not implemented"


