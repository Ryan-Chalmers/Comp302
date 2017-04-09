module midterm_code

let inc(n) = n + 1

//let repeat(f, n) = f>>f

let double(f) = fun x -> f(f x)
let triple(f) = fun x-> f(double x)
double inc
let rec repeat(f, n:int) = 
    match n with
    | 1 -> f
    | n -> 
        f>>repeat(f, n-1)

// let rec bits n = 
//     if(n=0) then [[]]
//     elif(n=1) then [[0];[1]]]
//     else
//         bits(n-1)::
let data = [[0];[1]]

let a = data |> List.map (fun x -> [0]@x)
let b = data |> List.map (fun x -> [1]@x)

a@b