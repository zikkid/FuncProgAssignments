
//Exercise 2.1
let downto1 n : int =
    if n > 0
    then [n .. -1 .. 1]
    else [];;

let downto2 n : int = match n with
    | n when n > 0 -> [n .. -1 .. 1]
    | n -> [];;

downto1 13;;
downto2 12;;

//Exercise 2.2
let removeOddIdx (xs: 'a list) =
    [for i in 0 .. 2 .. xs.Length - 1 -> xs.[i]];;
//would like to do this with pattern matching

removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
"was"; "white"; "as"; "snow"];;

//Exercise 2.3
let combinePair (xs: 'a list) =
    [for i in 1 .. 2 .. xs.Length - 1 -> (xs.[i-1], xs.[i])];;
//would like to do this with pattern matching

combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
"was"; "white"; "as"; "snow"];;
