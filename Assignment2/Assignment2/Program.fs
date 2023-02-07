
//Exercise 2.1
let downto1 n =
    if n > 0
    then [n .. -1 .. 1]
    else [];;

let downto2 n = match n with
    | n when n > 0 -> [n .. -1 .. 1]
    | n -> [];;

//downto1 13;;
//downto2 12;;

//Exercise 2.2
let removeOddIdx (xs: 'a list) =
    [for i in 0 .. 2 .. xs.Length - 1 -> xs.[i]];;
//would like to do this with pattern matching

//Exercise 2.3
let combinePair (xs: 'a list) =
    [for i in 1 .. 2 .. xs.Length - 1 -> (xs.[i-1], xs.[i])];;
//would like to do this with pattern matching

//Exercise 2.4
//not even gonna try

//Exercise 2.5
let explode1 (s: string) = Seq.toList s;;

//Exercise 2.6
let implode (cs: 'a list) = List.fold (fun s e -> s + e) "" cs;;


//Exercise 2.7