module Dict

    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
        
    let empty () = Leaf false
    
    let rec insert (s: string) =
        function
        | Leaf _            when s.Length = 0   ->  Leaf true
        | Node (_, dict)    when s.Length = 0   ->  Node (true, dict)
        
        //| Leaf b -> None
            
        | Node (b, dict) ->
            match dict.TryGetValue s.[0] with
            | (false, _) ->
                insert s.[1..s.Length] (empty ())
            | _ ->
                

    // let lookup x =
    //     | ??
    //     
    // let step x =
    //     | ??

