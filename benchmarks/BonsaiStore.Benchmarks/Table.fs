namespace FSharp.BonsaiStore.Benchmarks

module Table =

    /// Table type.
    type Table<'K,'V when 'K : comparison> = Map<'K,'V>
    
    /// Empty table
    let empty = Map.empty
    
    /// Constructs a table with a single key-value pair.
    let single k v = Map.add k v empty

    /// Merges a sequence of tables
    let mergeSeq (ts: seq<Table<'K,float>>) = 
        let dict = System.Collections.Generic.Dictionary<_,_>()
        for t in ts do
            for kv in t do
                match dict.TryGetValue kv.Key with
                | (true,v)      -> dict.[kv.Key] <- v + kv.Value
                | _             -> dict.Add(kv.Key, kv.Value) 
        dict
        |> Seq.map (fun kv -> (kv.Key, kv.Value))
        |> Map.ofSeq

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


    /// Top n from table.
    let top n (table: Table<'K,float>) =
        table
        |> Map.toSeq
        |> Seq.sortBy (fun (b,v) -> -v)
        |> Seq.truncate n
        |> Map.ofSeq

    let showTable<'K, 'V when 'K : comparison>  (table: Table<'K,'V>)  =
        if table.Count = 0 then
            printfn "Empty table"
        else
            let maxKey =  
                let k = table |> Seq.maxBy (fun kv -> (kv.Key.ToString()).Length)
                (k.Key.ToString()).Length
            for kv in table do
                let spcs = List.replicate (maxKey + 4 - (kv.Key.ToString()).Length) " "
                let space = System.String.Join("", spcs)
                printfn "%s%s%A" (kv.Key.ToString()) space kv.Value