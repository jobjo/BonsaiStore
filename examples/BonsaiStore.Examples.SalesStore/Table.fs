namespace FSharp.BonsaiStore.Examples

module Table =

    type Table<'K,'V when 'K : comparison> = 
        { ElementCount : int; Table : Map<'K,'V> }
    let empty = {ElementCount = 0; Table = Map.empty}

    /// Merges a list of tables
    let merge (ts: seq<Table<'K,float>>) = 
        let dict = System.Collections.Generic.Dictionary<_,_>()
        let numDeals = ref 0
        for t in ts do
            numDeals := numDeals.Value + t.ElementCount
            for kv in t.Table do
                if dict.ContainsKey kv.Key then
                    dict.[kv.Key] <- dict.[kv.Key] + kv.Value
                else
                    dict.Add(kv.Key, kv.Value)
        let table =
            Map.ofList [ for kv in dict do yield (kv.Key, kv.Value) ]
        { ElementCount = numDeals.Value; Table = table}

    let fromSeq xs = {ElementCount = Seq.length xs; Table = Map.ofSeq xs}

    /// Top n from table.
    let top n (table: Table<'K,float>) =
        table.Table
        |> Map.toSeq
        |> Seq.sortBy (fun (b,v) -> -v)
        |> Seq.truncate n

    let showTable<'V>(table: Table<string,'V>) =
        if table.ElementCount = 0 then
            printfn "Empty table"
        else
            let maxKey =  
                let k = table.Table |> Seq.maxBy (fun kv -> kv.Key.Length)
                k.Key.ToString().Length
            for kv in table.Table do
                let spcs = List.replicate (maxKey + 4 - kv.Key.Length) " "
                let space = System.String.Join("", spcs)
                printfn "%s%s%A" (string kv.Key) space kv.Value