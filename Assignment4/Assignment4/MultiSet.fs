module MultiSet
    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>
     
    //Green
    let empty = M Map.empty
    
    let size (M s) = Map.fold (fun x k v -> x + v) 0u s
    let isEmpty m =
        match size m with
        | 0u -> true
        | _ -> false
    let contains a (M s) = Map.containsKey a s
    let numItems a (M s) =
        match Map.tryFind a s with
        | None -> 0u
        | Some x -> x
    let add a n (M s) =
        match Map.tryFind a s with
        | Some an -> Map.add a (an + n) s |> M
        | None -> Map.add a n s |> M   
    let addSingle a s = add a 1u s
    let remove a n (M s) =
        match Map.tryFind a s with
        | Some an when an <= n -> Map.add a 0u s |> M
        | Some an -> Map.add a (an - n) s |> M
        | None -> s |> M
    let removeSingle a (M s) =
        match Map.tryFind a s with
        | Some an -> Map.add a (an - 1u) s |> M
        | None -> s |> M
        
        
        //Map.add a -1u s |> M
    let fold f acc (M s) = Map.fold f acc s
    let foldBack f (M s) acc = Map.foldBack f s acc
         
         //Yellow
         //let ofList lst = List.fold (fun acc x -> addSingle x acc) empty lst
         //let toList (M s) = fold
         
         