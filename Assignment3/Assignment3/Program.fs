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
//Consider only following types of aExp:
//N, Add, Sub, Mul
let rec arithEvalSimple aExp =
    match aExp with
    | N a -> a
    | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a, b) -> arithEvalSimple a * arithEvalSimple b;;

//Exercise 3.2
//Consider only following types of aExp:
//N, V, Add, Sub, Mul
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

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

//Exercise 3.3
//Consider all types of aExp
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
    | CV aExp -> fst word.[arithEval aExp word state];;         //Note: word is a list of tuples (char, PV)
                                                                //arithEval of an int N will return N
                                                                //lookup at word.[N] returns the tuple at index N
                                                                //the 'fst' value of that tuple will be the char
    
    
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

//Exercise 3.6
let isConsonant cExp =
    Conj(Not(IsVowel cExp), IsLetter(cExp));;

//Exercise 3.7
type stmnt =    
    | Skip                          //Skip
    | Ass of string * aExp          //Variable assignment
    | Seq of stmnt * stmnt          //Sequential composition
    | ITE of bExp * stmnt * stmnt   //If-then-else statement
    | While of bExp * stmnt;;       //While statement
    
let rec evalStmnt stm (word: word) (state: Map<string, int>) =
    match stm with
    | Skip -> state
    | Ass (x, aExp) -> Map.add x (arithEval aExp word state) state
    | Seq (stm1, stm2) ->
        evalStmnt stm1 word state |>
        evalStmnt stm2 word
    | ITE (bExp, stm1, stm2) ->
        if boolEval bExp word state
        then evalStmnt stm1 word state
        else evalStmnt stm2 word state
    | While (bExp, stm) ->
        match boolEval bExp word state with
        | true -> evalStmnt (While (bExp, stm)) word (evalStmnt stm word state)
        | _ -> state;;
        
evalStmnt Skip [] Map.empty;;
evalStmnt (Ass ("x", N 5)) [] Map.empty;;
evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty;;
evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;
evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;;

evalStmnt (While (V "x" .<=. WL,
    Seq (Ass ("y", V "y" .+. V "x"),
        Ass ("x", V "x" .+. N 1))))
    hello Map.empty;;
    
evalStmnt (While (V "x" .<=. WL,
    Seq (Ass ("y", V "y" .+. V "x"),
        Ass ("x", V "x" .+. N 1))))
    hello (Map.ofList [("x", 3); ("y", 100)]);;
    
//Exercise 3.8
type squareFun = word -> int -> int -> int;;

let stmntToSquareFun stm (word: word) pos acc =
    evalStmnt stm word (Map.ofList [("_pos_", pos); ("_acc_", acc)]) |>
    Map.find "_result_";;


let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore));;
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore));;
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore));;
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore));;
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore));;

let containsNumbers =
    stmntToSquareFun
        (Seq (Ass ("_result_", V "_acc_"),
            While (V "i" .<. WL,
                ITE (IsDigit (CV (V "i")),
                    Seq (
                        Ass ("_result_", V "_result_" .*. N -1),
                        Ass ("i", WL)),
                    Ass ("i", V "i" .+. N 1)))));;

singleLetterScore hello 0 0;;
doubleLetterScore hello 0 0;;
tripleLetterScore hello 0 0;;
singleLetterScore hello 0 42;;
doubleLetterScore hello 0 42;;
tripleLetterScore hello 0 42;;
containsNumbers hello 5 50;;
containsNumbers (('0', 100)::hello) 5 50;;
containsNumbers (hello @ [('0', 100)]) 5 50;;

//Exercise 3.9
let oddConsonants = stmntToSquareFun 