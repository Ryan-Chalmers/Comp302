namespace Test

module foo =   

    let list = [1.0;2.0;3.0;4.0;5.0] 

    let rec sum l:float = 
        match l with
        |[]-> (0.0)
        |x::xs-> x + sum(xs)
        //x is the first element in the list and xs is the remaining list

    let rec rev l =
        match l with
        | [] -> []
        | x::xs -> rev(xs) @ [x] 