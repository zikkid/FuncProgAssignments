module MultiSet
     type MultiSet<'a> when 'a : comparison = M of Map<'a, uint32>
     
     let empty = Map.empty<'a, uint32> //Map.<'a, uint32> []
     let isEmpty (M s) = Map.isEmpty s
     let size (M s) = Map.count s
     let contains a (M s) = Map.containsKey a s
     let numItems a (M s) =
        match Map.tryFind a s with
        | _ -> true
     let add a n (M s) =
         let an = Map.find a s
         Map.add a (an + n) s
     let addSingle a (M s) = Map.add a s
     let remove a n (M s) =
        let an = Map.find a s
        Map.add a (-(an + n)) s
     let removeSingle a (M s) = Map.remove a s
     let fold f acc (M s) = Map.fold f acc s
     let foldBack f acc (M s) = Map.foldBack f s acc