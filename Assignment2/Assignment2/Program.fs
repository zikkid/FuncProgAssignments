
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
let implode (cs: char list) = List.fold (fun s e -> s + string e) "" cs;;

let implodeRev (cs: char list) = List.fold (fun s e -> string e + s) "" cs;;

// implode ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'];;
// implodeRev ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!'];;

//Exercise 2.7
let toUpper (s: string) =
    s
    |> explode1
    |> Seq.map System.Char.ToUpper
    |> Seq.map string
    |> String.concat "";;
    
let toUpper2 (s: string) = String.map System.Char.ToUpper s;;

let toUpper3 (s: string) =
    s
    |> explode1
    |> Seq.map System.Char.ToUpper
    |> Seq.toList
    |> implode;;
    

// toUpper "sup dawg";;
// toUpper2 "sup dawg";;
// toUpper3 "sup dawg";;

//Exercise 2.8
let rec ack (m, n) =    match (m, n) with
                        | (0, n) -> n + 1
                        | (m, 0) when m > 0 -> ack (m - 1, n)
                        | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1));;
                        
//Exercise 2.9
type complex = float * float;;

let mkComplex (a: float) (b: float) : complex = (a, b);;
let mkComplex2 (a: float) (b: float) = complex (a, b);;

let complexToPair (x: complex) = (fst x, snd x);;

let addition (a: complex) (b: complex) = (fst a + fst b, snd a + snd b);;
let multiplication (a: complex) (b: complex) = (fst a ** fst b - snd a ** snd b, snd a ** fst b + fst a ** snd b);;
// let subtraction (a: complex) (b: complex) = ;;