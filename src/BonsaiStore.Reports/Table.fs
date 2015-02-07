namespace FSharp.BonsaiStore.Reports
module Table =

    /// Table is a standard Map.
    type Table<'K,'V when 'K : comparison> = Map<'K,'V>

    /// Empty table
    let empty = Map.empty
    
    /// Constructs a table with a single key-value pair.
    let single k v = Map.add k v empty

    /// Table from a sequence of key-value pairs.
    let fromSeq xs = Map.ofSeq xs

    /// Retrieve all elements of a table.
    let elements m = Map.toList m

    /// Select the top n elements given an ordering criterion.
    let topBy n f =
        elements >> List.sortBy (snd >> f) >> List.rev >> Seq.truncate n >> List.ofSeq

    /// Select top n elements from a table.
    let top n =
        elements >> List.sortBy snd >> List.rev >> Seq.truncate n >> List.ofSeq

    /// Merges a sequence of tables
    let mergeWith<'K,'V when 'K : comparison> (merge: 'V -> 'V -> 'V) (ts: seq<Table<'K,'V>>) =
        let dict = System.Collections.Generic.Dictionary<_,_>()
        for t in ts do
            for kv in t do
                match dict.TryGetValue kv.Key with
                | (true,v)      -> dict.[kv.Key] <- merge v kv.Value
                | _             -> dict.Add(kv.Key, kv.Value) 
        dict
        |> Seq.map (fun kv -> (kv.Key, kv.Value))
        |> Map.ofSeq