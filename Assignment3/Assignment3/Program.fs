// type aExp =
//     | N of int
//     | V of string
//     | Add of aExp * aExp
//     | Sub of aExp * aExp
//     | Mul of aExp * aExp;;
    
type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp;;

let (.+.) a b = Add (a, b);;
let (.-.) a b = Sub (a, b);;
let (.*.) a b = Mul (a, b);;

let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;

//Exercise 3.1
let rec arithEvalSimple aExp =
    match aExp with
    | N a -> a
    | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a, b) -> arithEvalSimple a * arithEvalSimple b
    | _ -> failwith "todo";;

//Exercise 3.2
let rec arithEvalState aExp (state: Map<string, int>) =
    match aExp with
    | N a -> a
    | V a ->
        match a with
        | a when state.ContainsKey a -> state.[a]
        | _ -> 0
    | Add (a, b) -> arithEvalState a state + arithEvalState b state
    | Sub (a, b) -> arithEvalState a state - arithEvalState b state 
    | Mul (a, b) -> arithEvalState a state * arithEvalState b state;;

arithEvalState a6 (Map.ofList [("x", 5)]);;
arithEvalState a6 (Map.ofList [("y", 5)]);;


//Copy paste from Assignment 3 sheet
// type aExp =
//     | N of int // Integer value
//     | V of string // Variable
//     | WL // Length of the word
//     | PV of aExp // Point value of character at specific word index
//     | Add of aExp * aExp // Addition
//     | Sub of aExp * aExp // Subtraction
//     | Mul of aExp * aExp;; // Multiplication

type word = (char * int) list;;

//Exercise 3.3
let rec arithEval aExp (word: word) (state: Map<string, int>) = 
    match aExp with
    | N int -> int
    | V str ->
        match str with
        | str when state.ContainsKey str -> state.[str]
        | _ -> 0
    | WL -> word.Length
    | PV a -> snd word.[arithEval a word state]
    | Add (a, b) -> arithEval a word state + arithEval b word state
    | Sub (a, b) -> arithEval a word state - arithEval b word state
    | Mul (a, b) -> arithEval a word state * arithEval b word state;;

let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)];;
arithEval WL hello Map.empty;;

//Exercise 3.4
type cExp =
    | C of char         //Character value
    | ToUpper of cExp   //Converts lowercase char to uppercase
    | ToLower of cExp   //Converts uppercase char to lowercase
    | CV of aExp;;      //Character lookup at index

let rec charEval cExp (word: word) (state: Map<string, int>) = 
    match cExp with
    | C char -> char
    | ToUpper c -> System.Char.ToUpper(charEval c word state)
    | ToLower c -> System.Char.ToLower(charEval c word state)
    | CV aExp -> fst word.[arithEval aExp word state];;
    
    
//Exercise 3.5
type bExp =
    | TT                    //True
    | FF                    //False
    
    | AEq of aExp * aExp    //Numeric equality
    | ALt of aExp * aExp    //Numeric less than
    
    | Not of bExp           //Boolean not
    | Conj of bExp * bExp   //Boolean conjunction
    
    | IsDigit of cExp       //Check for digit
    | IsLetter of cExp      //Check for letter
    | IsVowel of cExp;;     //Check for vowel
    
let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)           (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b)                    (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)      (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b)                    (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b)     (* numeric greater than *)

let vowels = ['A'; 'E'; 'I'; 'O'; 'U'];;
let isVowel (c: char) =
    List.exists (fun x -> x = System.Char.ToUpper(c)) vowels;;
let rec boolEval bool (word: word) (state: Map<string, int>) =
    match bool with
    | TT -> true
    | FF -> false
    
    | AEq (a, b) -> arithEval a word state = arithEval b word state
    | ALt (a, b) -> arithEval a word state < arithEval b word state
    
    | Not bool -> not (boolEval bool word state)
    | Conj (a, b) -> boolEval a word state && boolEval b word state
    
    | IsDigit cExp -> System.Char.IsDigit(charEval cExp word state)
    | IsLetter cExp -> System.Char.IsLetter(charEval cExp word state)
    | IsVowel cExp -> isVowel(charEval cExp word state);;
    
isVowel 'e';;
isVowel 'B';;

//Exercise 3.