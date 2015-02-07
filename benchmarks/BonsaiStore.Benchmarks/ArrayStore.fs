namespace FSharp.BonsaiStore.Benchmarks
module ArrayStore =
    open FSharp.BonsaiStore

    let rec buildStore items =
        let report filter map reduce =
            let pred = Quotations.compileQuotationFilter filter
            items
            |> Array.Parallel.choose (fun x ->
                if pred x then 
                    Some (map x)
                else 
                    None
            )
            |> reduce
        { new IStore<'T> with
            member this.Report filter map reduce = report filter map reduce 
            member this.Filter f = Array.filter f items |> buildStore
            member this.Insert xs = Array.append items (Array.ofSeq xs) |> buildStore
            member this.Map f = Array.map f items |> buildStore
        }


