module Dict

    type Dict =
        | Leaf of bool
        | Node of bool * Map<char, Dict>
        
    let empty () = Leaf false
    
    let insert (s: string) (dict: Dict) =
        function
        | Leaf _            when s.Length = 0   ->  Leaf true
        | Node (_, dict)    when s.Length = 0   ->  Node (true, dict)
        
        | Leaf b ->
            
        | Node b ->
            
    let lookup (s: string) (dict: Dict) =
        function
        | 
        
    // let lookup x =
    //     | ??
    //     
    // let step x =
    //     | ??

