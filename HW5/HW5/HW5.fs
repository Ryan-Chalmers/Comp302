(* You may need the line below or not depending on how you are testing
things.  Keep it or remove it as you want.  We will insert what we need for
our testing purposes.  We do not need any other header information like
your name or student id as myCourses tracks this for us. *)
module HW5

(* This is the type definition for the expressions that we will produce as a result
of parsing the input strings. *)

type exptree = Var of char | Expr of char * exptree * exptree

(* We only allow lower-case one-character variable names*)
let charSet = ['a' .. 'z']

(* Here is an example input string.  Blank spaces are not allowed. *)
let example = "(h*(x+(b*z)+j))*c"

(* This just tests if the character is one of the allowed variable names.*)
let isin (x: char) L = List.exists (fun y -> x = y) L

(* This is the top-level function.  It reads an input string and outputs the parse tree.
It is not recursive at top level but the main guts of it consists of three
mutually-recursive functions called expr, term and primary.  There is also a function
called getsym which returns the next character from the input string.  getsym is imperative
and uses the mutable local variables sym and cursor.  Please do NOT change the definition of
getsym or the declarations of sym and cursor.  No doubt all this can be done more slickly,
but I am trying to be as simple-minded as possible. *)

let parse (inputexp: string): exptree = 
  let sym = ref inputexp.[0]
  let cursor = ref 0

  let getsym () =
    cursor := !cursor + 1
    sym := inputexp.[!cursor]


  (*Took this straight from lecture notes on march 9th*)
  let rec expr (): exptree = 
    let mutable t = term()
    while !sym = '+' do
        getsym()
        t <- Expr ('+', t, term())
    t

  (*Took this straight from lecture notes on march 9th*)
  and term (): exptree = 
    let mutable p = primary()
    while !sym = '*' do
        getsym()
        p <- Expr ('*', p, primary())
    p

  and primary (): exptree =  //I did this for you.
    if !sym = '(' then
      getsym()
      let result = expr ()
      if not (!sym = ')') then 
        failwith "Mismatched parens"
      else  
        if (!cursor = inputexp.Length - 1) 
        then 
          result
        else 
          getsym()
          result
    elif (isin !sym charSet) then 
      if (!cursor = inputexp.Length - 1) 
      then 
        (Var !sym) 
      else 
        let result = Var !sym in (getsym(); result)
    else
      printfn "sym is : %c." !sym
      failwith "In primary"
  expr() //This is part of the top-level function parse.

let e = parse example

//Simple in order traversal to check in see if my parse function is working
let rec inorder(e: exptree) = 
  match e with 
  | Var(v) -> printfn "%c" v
  | Expr(c, lc, rc) -> inorder(lc); printfn "%c" c; inorder(rc)  
(* Now for Question 2 *)

(*  Do not change this.  tempstore will be a global variable.  Remember to reset this between
tests of the program. *)
let mutable tempstore = 0

(*Basically what we do here is an inorder traversal of the expression tree. Everytime we hit a Left child node
that is also a leaf we want to load that node. These will be sent to the helper program 
in the for (Var V, $). Then when we look at the middle node we want to check if its left child and its right child
are a leaf and a tree respectively, if so we want to store. Once we reach a right child we want to either add or 
multiply it. Once both the left and right child nodes have been evaluated we check to see if we need to add or multply 
something from the store*)
let codegen (e: exptree) = 
  let rec helper (e: exptree, tag: char) = //failwith "---Not implemented---"
    (* Code for helper goes here. *)
    if(tag = '$') then
      match e with 
      | Var (v) -> printfn "LOAD %c" v
      | Expr (c, lc, rc) -> 
        helper(lc, '$');
        match rc with //if left child is a Var and right is an Expr then Store
        | Var(v) -> helper(rc, c);
        | Expr(c,l,r) -> 
          tempstore <- tempstore + 1;
          printfn "STORE %i" tempstore; 
          helper(rc, c);
        if(c = '*' && tempstore > 0) then 
          printfn "MUL %i" tempstore;
          tempstore <- tempstore - 1;
        if(c = '+' && tempstore > 0) then 
          printfn "ADD %i" tempstore;
          tempstore <- tempstore - 1;
    if(tag = '*') then
      match e with
      | Var (v) ->
        printfn "MUL %c" v
      | Expr (c, lc, rc) -> 
        helper(lc, '$');
        helper(rc, c);
    if(tag = '+') then
      match e with
      | Var (v) -> 
        printfn "ADD %c" v
      | Expr (c, lc, rc) ->
        helper(lc, '$'); 
        helper(rc, c);
  helper(e,'$') //This is part of the top-level function codegen.  Do not change it.


inorder e

codegen e