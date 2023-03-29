module Eval

    open System
    open StateMonad
    open Types

    (* Code for testing *)

    //my definition of hello begins
    type word = (char * int) list;;
    let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)];;
    //my definition of hello ends
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b =
        a >>= (fun x -> b >>= (fun y -> ret (x + y)))
        
    let div a b =
        a >>= (fun x -> b >>= (fun y ->
            match (x, y) with
            | _, y when y = 0 -> fail DivisionByZero
            | x, y when y <> 0 -> ret (x / y)))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char             (* Character value *)
       | CV of aExp             (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV x -> arithEval x >>= (fun r -> pointValue r)
        | Add (x, y) -> add (arithEval x) (arithEval y)
        | Sub (x, y) ->
            arithEval x >>= (fun a -> arithEval y >>= (fun b ->
                ret (a - b)))
        | Mul (x, y) ->
            arithEval x >>= (fun a -> arithEval y >>= (fun b ->
                ret (a * b)))
        | Div (x, y) -> div (arithEval x) (arithEval y)
        | Mod (x, y) ->
            arithEval x >>= (fun a -> arithEval y >>= (fun b ->
                match (a, b) with
                | _, b when b = 0 -> fail DivisionByZero
                | a, b -> ret (a % b)))
        | CharToInt c -> charEval c >>= (fun r -> ret (int r))

    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV c -> arithEval c >>= (fun r -> characterValue r)
        | ToUpper c -> charEval c >>= (fun r -> ret (Char.ToUpper r))
        | ToLower c -> charEval c >>= (fun r -> ret (Char.ToLower r))
        | IntToChar n -> arithEval n >>= (fun r -> ret (char r))
         

    and boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq (x, y) ->
            arithEval x >>= (fun a -> arithEval y >>= (fun b ->
                ret (( = ) a b)))
        | ALt (x, y) ->
            arithEval x >>= (fun a -> arithEval y >>= (fun b ->
                ret (( < ) a b)))

        | Not x -> boolEval x >>= (fun r -> ret (not r))
        | Conj (x, y) ->
            boolEval x >>= (fun a -> boolEval y >>= (fun b ->
                ret (( && ) a b)))

        | IsVowel c -> charEval c >>= (fun r -> ret ("AEIOUY".Contains (Char.ToUpper r)))
        | IsLetter c -> charEval c >>= (fun r -> ret (Char.IsLetter r))
        | IsDigit c -> charEval c >>= (fun r -> ret (Char.IsDigit r))


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    