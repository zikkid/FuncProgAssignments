module MultiSet
     type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>
     
     //Green
     let empty = M Map.empty
     let isEmpty (M s) = Map.isEmpty s
     let size (M s) = uint32 (Map.count s) 
     let contains a (M s) = Map.containsKey a s
     let numItems a (M s) =
        match Map.tryFind a s with
        | None -> 0u
        | Some x -> x
     let add a n (M s) =
         let an = Map.find a s
         Map.add a (an + n) s |> M
     let addSingle a s = add a 1u s
     let remove a n (M s) =
        let an = Map.find a s
        match an with
        | an when an <= n -> Map.add a 0u s |> M
        | _ -> Map.add a (an - n) s |> M
     let removeSingle a (M s) = Map.remove a s |> M
     let fold f acc (M s) = Map.fold f acc s
     let foldBack f (M s) acc = Map.foldBack f s acc
     
     //Yellow
     let ofList lst = List.fold (fun acc x -> addSingle x acc) empty lst
     let toList (M s) = fold 