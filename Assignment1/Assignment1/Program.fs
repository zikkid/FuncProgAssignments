module Assignment1.Program

//Exercise 1.1
let sqr = function x -> x * x;;

//Exercise 1.2
//Testing wont allow input of type (x, n)
//let pow = function (x, n) -> System.Math.Pow(x, n);;
let pow x n = System.Math.Pow(x, n);;

//Exercise 1.3
let rec sum = function
    | 0 -> 0
    | n -> n + sum(n - 1);;

//Exercise 1.4
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2);;

//Exercise 1.5
let dup = function s -> s + "" + s;;

//Exercise 1.6
let rec dupn s n =  match (s, n) with 
                    | (s, 0) -> ""
                    | (s, 1) -> s
                    | (s, n) -> s + dupn s (n - 1);;

//Exercise 1.7
let rec bin (n, k) =    match (n, k) with
                        | (n, 0) -> 1
                        | (n, k) when n = k -> 1
                        | (n, k) -> bin((n - 1), (k - 1)) + bin((n - 1), k);;

//Exercise 1.8
let timediff t1 t2 = (fst t2 - fst t1) * 60 + snd t2 - snd t1;;

//Exercise 1.9
let minutes t = timediff(0, 0) (fst t, snd t);;

//Exercise 1.10
let curry (f: 'a * 'b -> 'c) a b = f (a, b);;
let uncurry (f: 'a -> 'b -> 'c) (a, b) = f a b;;

//Exercise 1.11
let empty (char, pointValue) = function pos -> (char, pointValue);;
let theLetterA : int -> char * int = empty ('A', 1);;

//Exercise 1.12
let add newPos (char, pointValue) word =  function pos ->
                                          if pos = newPos then (char, pointValue)
                                          else word pos;;
                            
let theLettersAB = add 1 ('B', 3) theLetterA;;


//Exercise 1.13
let hello : int -> char * int =
    empty ('H', 4) |>
    add 1 ('E', 1) |>
    add 2 ('L', 1) |>
    add 3 ('L', 1) |>
    add 4 ('O', 1);;

//Exercise 1.14
let singleLetterScore word pos = snd (word pos);;
let doubleLetterScore word pos = (singleLetterScore word pos) * 2;;
let tripleLetterScore word pos = (singleLetterScore word pos) * 3;;

singleLetterScore hello 0;;