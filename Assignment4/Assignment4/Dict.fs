module Dictionary

    type Dictionary =
        | Leaf of bool
        | Node of bool * Map<char, Dictionary>
        
    let empty () = Leaf false
    
    let rec insert (s: string) =
        function
        | Leaf _            when s.Length = 0   ->  Leaf true
        | Node (_, map)     when s.Length = 0   ->  Node (true, map)
        
        | Leaf b ->
            Node (b, Map.add s.[0] (insert s.[1..] (empty ())) Map.empty)
            
        | Node (b, map) ->
            match Map.tryFind s.[0] map with
            | Some d -> Node (b, Map.add s.[0] (insert s.[1..] (d)) map)
            | None -> Node (b, Map.add s.[0] (insert s.[1..] (empty ())) map)
            
    let rec lookup (s: string) =
        function
        | Leaf b            when s.Length = 0   ->  b
        | Node (b, map)     when s.Length = 0   ->  b
        
        | Leaf _                                -> false
        | Node (b, map) ->
            match Map.tryFind s.[0] map with
            | Some d -> lookup s.[1..] d
            | None -> false
            
    let step (c: char) =
        function
        | Leaf _ -> None
        | Node (_, map) ->
            match Map.tryFind c map with
            | Some d ->
                let getBool d =
                    match d with
                    | Leaf b -> b
                    | Node (b, _) -> b
                Some (getBool d, d)
            | None -> None
