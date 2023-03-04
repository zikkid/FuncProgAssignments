//Exercise 5.1
let rec sum acc n =
    match n with
    | 0 -> acc
    | _ -> sum (acc + n) (n - 1)


//Exercise 5.2
let rec length acc lst =
    match lst with
    | [] -> acc
    | x::xs -> length (acc + 1) xs
    
//Exercise 5.3
let rec foldBack f acc lst =
    match lst with
    | [] -> f acc
    | x::xs -> foldBack f () xs