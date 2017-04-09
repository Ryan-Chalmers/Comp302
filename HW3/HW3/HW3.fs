module Hw3

(* Assignment 3 *) (* Do not edit this line. *)
(* Student name: Ryan Chalmers, Id Number: 260581055 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops. *)

(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref

let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}

(* This converts an RList to an ordinary list, which is then displayed. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This converts a cell to a list.  You may find it useful for testing.  No need to
use it in your solution. *)

let cellToRList (c:Cell):RList = ref (Some c)

(*To reverse the list we first pass in the head to the function and initial a ref cell that points to 
the reversed portion of the list. Next we match the input list with either None or Some. If we reach None
the previous node was the last Node and we can return the rev list. If we match with Some then we first want
to save the next cell to s. Now we can change the current nodes next RList to the previous, already reversed
RList. Next we want to update the return lst to be equal to the remaining work that needs to be done reversing
the list. When this is complete we will return rev to the lst and then return lst*)

(* This is what you need to code. *)
let reverse (lst: RList) = 
  let rev = ref None
  let rec helper lst: RList   = 
    match !lst with 
    | None -> rev 
    | Some l -> 
      let s = !(l.next) 
      l.next := !rev 
      rev := !lst 
      lst := !(helper (ref s)) 
      lst
  helper(lst)


(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance | ChangePassword of string | Close
(*The functions for Withdrawal, Deposit, and CheckBalance were ripped directly from the Objects handout.
The fundamental change I made to the program was implementing a check to see if the password stored in 
the object is equal to that which the user defined before allowing any changes to be made. Additionally
I implemented a check to make sure a new ref variable accountStatus. If Account status is closed then the
user is blocked from altering the accoint and additiionally cannot see whether the correct password has been
entered as the account is closed.*)
let makeProtectedAccount(openingBalance: int, password: string) = 
  let balance = ref openingBalance
  let pw = ref password
  let accountStatus = ref 0
  fun (p: string, t:transaction) ->
    if(p = !pw && !accountStatus = 0) then
      match t with 
      | Withdraw(m) -> if(!balance > m)
                          then balance:= !balance-m; printfn "Balance is %i" !balance
                        else 
                          printfn("Insufficient Funds")
      | Deposit(m) -> balance:= !balance+m
                      printfn "Balance is %i" !balance
      | CheckBalance -> printfn "Balance is %i" !balance 
      | ChangePassword(newp) -> pw:=newp; printfn "Password Changed"
      | Close -> accountStatus := 1; printfn "Account Closed"
    else
      if(!accountStatus = 1) then
        printfn "Account Closed"
      else printfn "Incorrect Password"
      
// let Ryan = makeProtectedAccount(100, "password")
// Ryan("password", Deposit(20))
// Ryan("passwd", Withdraw(100))
// Ryan("password", CheckBalance)
// Ryan("password", ChangePassword("newpassword"))
// Ryan("password", Deposit(20))
// Ryan("newpassword", Deposit(20))
// Ryan("newpassword", Close)
// Ryan("newpassword", Deposit(20))
// Ryan("password", Deposit(20))

(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

(*Here we first want to define a q that parametric in 'a. Once that is completed we 
add the first not into the queue. The goal now is to continue running the Breadth First
Traversal until the queue is empty. The Queue will change size as we run the function but
we will continue to run the loop until it is empty. We start by Dequeuing the first 
Node. We then take all the children of that Node and queue them. once that is completed 
we run the function on the data contained in the Dequeued Node. Repeating the loop will 
successfully complete a depth first traversal*)
let bfIter f ltr =
  let q = new Queue<ListTree<'a>>()
  q.Enqueue(ltr) 
  while q.Count > 0 do
    match q.Dequeue() with
    | Node (d,l) -> 
      for j in l do
        q.Enqueue(j)
      f d


(* Some examples I used for testing.  *)
let n5 = Node(5,[])
let n6 = Node(6,[])
let n7 = Node(7,[])
let n8 = Node(8,[])
let n9 = Node(9,[])
let n10 = Node(10,[])
let n12 = Node(12,[])
let n11 = Node(11,[n12])
let n2 = Node(2,[n5;n6;n7;n8])
let n3 = Node(3,[n9;n10])
let n4 = Node(4,[n11])
let n1 = Node(1,[n2;n3;n4])



(* Just for testing, not needed for your solution. *)

let showNode n =
  match n with
    | Node(i,_) -> (printfn "%i" i)
    
bfIter (fun i -> printfn "%i" i) n1