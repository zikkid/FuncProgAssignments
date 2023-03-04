module Dict

    type Dict<'T when 'T : comparison> =
        | Leaf of bool
        | Node of bool * Map<char, Dict<'T>>
        
    let empty () = Leaf false
    
    let rec insert (s: string) =
        function
        | Leaf _            when s.Length = 0   ->  Leaf true
        | Node (_, dict)    when s.Length = 0   ->  Node (true, dict)
        
        //| Leaf b -> None
            
        | Node (b, dict) ->
            match Map.add s dict with
            | (false, _) ->
                dict.[s[0]] <- insert s.[1..s.Length] (empty ())
                Node (b, dict)
            | _ ->
                insert  
                

    // let lookup x =
    //     | ??
    //     
    // let step x =
    //     | ??

