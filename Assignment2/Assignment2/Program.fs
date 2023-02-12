
//Exercise 2.1
let downto1 (n: int) =
    if n > 0
    then [n .. -1 .. 1]
    else [];;

let downto2 (n: int) = match n with
                        | n when n > 0 -> [n .. -1 .. 1]
                        | _ -> [];;

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
type complex = float * float;;

let mkComplex (x: float) (y: float) : complex = (x, y);;
let mkComplex2 (x: float) (y: float) = complex (x, y);;
let complexToPair (x: complex) = (fst x, snd x);;

//has the correct form but doesnt guarantee complex output and isn't inline
//let addition (x: complex) (y: complex) =
//    (fst x + fst y, snd x + snd y);;

//upgraded to inline with a promised return type utilizing earlier built function mkComplex
let (|+|) (x: complex) (y: complex) : complex =
    mkComplex (fst x + fst y) (snd x + snd y);;

//has the correct form but doesn't guarantee complex output and isn't inline
//let multiplication (x: complex) (y: complex) =
//    (fst x ** fst y - snd x ** snd y, snd x ** fst y + fst x ** snd y);;

//upgraded to inline with a promised return type utilizing earlier built function mkComplex
let (|*|) (x: complex) (y: complex) : complex =
    mkComplex (fst x * fst y - snd x * snd y) (snd x * fst y + fst x * snd y);;

//let subtraction (x: complex) : complex =
//    mkComplex (-fst x) (-snd x);;
    
let (|-|) (y: complex) (x: complex) : complex =
    y |+| mkComplex (-fst x) (-snd x);;

//let division (x: complex) : complex =
//    mkComplex (fst x / fst x * fst x + snd x * snd x) (- snd x / fst x * fst x + snd x * snd x);;
    
let (|/|) (y: complex) ((a, b): complex) : complex =
    y |*| mkComplex (a / (a ** 2.0 + b ** 2.0)) (- b / (a ** 2.0 + b ** 2.0));;

//Exercise 2.5
let explode1 (s: string) = Seq.toList s;;
let explode2 (s: string) = Seq.toList s;;

explode2 "";

//Exercise 2.6
let implode (cs: char list) = List.fold (fun s e -> s + string e) "" cs;;

let implodeRev (cs: char list) = List.fold (fun s e -> string e + s) "" cs;;
//why doesnt this work
//let implodeRev2 (cs: char list) = List.foldBack (fun s e -> s + string e) "" cs;

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
                        | (m, 0) when m > 0 -> ack (m - 1, 1)
                        | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
                        | _ -> failwith "todo";;
                        
ack (3, 11);;
                        
//Exercise 2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start);;
    
time (fun () -> ack (3, 11));;

let timeArg1 f a =
    time (fun () -> f a);;

timeArg1 ack (3, 11);;

//Exercise 2.10
let rec downto3 f n e =
    if n <= 0
    then e
    else downto3 f (n - 1) (f n e);;

//let fac
//need help

//Exercise 2.11
type word = (char * int) list;;

let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)];;

//Exercise 2.12
type squareFun = word -> int -> int -> int;;

//why can't I specify with ": squareFun"
let singleLetterScore (word: word) pos accscore = (snd word.[pos]) + accscore;;

let doubleLetterScore (word: word) pos accscore = (snd word.[pos]) * 2 + accscore;;

let tripleLetterScore (word: word) pos accscore = (snd word.[pos]) * 3 + accscore;;

//Exercise 2.13
let doubleWordScore (word: word) pos accscore = accscore * 2;;

let tripleWordScore (word: word) pos accscore = accscore * 3;;

//Exercise 2.14
let oddConsonants (word: word) pos accscore =
    let mutable count = 0
    List.iter (fun (char, pointValue) -> count <- count + 1) word
    if count % 2 = 1
    then -accscore
    else accscore;;
    
//Putting square functions together
type square = (int * squareFun) list;;

let SLS : square = [0, singleLetterScore];;

let DLS : square = [0, doubleLetterScore];;

let TLS : square = [0, tripleLetterScore];;

let DWS : square = SLS @ [1, doubleWordScore];;

let TWS : square = SLS @ [1, tripleWordScore];;

//Exercise 2.15
let calculatePoints squares word =
    List.mapi (fun ) squares |>