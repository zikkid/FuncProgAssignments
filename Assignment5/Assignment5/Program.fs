//Green
//Exercise 5.1  
let sum m n =
    let rec aux acc =
        function
        | 0 -> acc + m
        | n -> aux (acc + (m + n)) (n - 1)
    aux 0 n

//Exercise 5.2
let length lst =
    let rec lengthA acc lst =
        match lst with
        | [] -> acc
        | x::xs -> lengthA (acc + 1) xs
    lengthA 0 lst
    
//Exercise 5.3    
let foldBack folder lst acc =
    let rec foldBackC lst c =
        match lst with
        | [] -> c acc
        | x :: xs -> foldBackC xs (fun r -> c (folder x r))
    foldBackC lst id

//Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x
    

let factC x =
    let rec aux c =
        function
        | 0 -> c 1
        | n -> aux (fun r -> c (r * n)) (n - 1) // r = fact (n - 1) * n
    aux id x
    
//Yellow
//Exercise 5.5
let fibA n =
    let rec aux acc1 acc2 =
        function
        | 0 -> acc1
        | x -> aux acc2 (acc1 + acc2) (x - 1)
    aux 0 1 n

let fibC n =
    let rec aux c =
        function
        | 0 -> fst (c (0, 1))
        | x -> aux (fun (acc1, acc2) -> c (acc2, (acc1 + acc2))) (x - 1)
    aux id n

//Exercise 5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)
    
bigListK id 130000