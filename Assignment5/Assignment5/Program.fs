//Exercise 5.1
let rec sum acc n =
    match n with
    | 0 -> acc
    | _ -> sum (acc + n) (n - 1)


//Exercise 5.2
let length lst =
    let rec lengthA acc lst =
        match lst with
        | [] -> acc
        | x::xs -> lengthA (acc + 1) xs
    lengthA 0 lst
    
//Exercise 5.3
let rec foldBack f acc lst =
    match lst with
    | [] -> f acc
    | x::xs -> foldBack f (foldBack f xs acc)
//Need help here

//Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x
    
let factC x =
    let rec aux 