namespace FSharp.BonsaiStore.Benchmarks
module ArrayStore =
    open FSharp.BonsaiStore
    let buildStore items =
        let report filter map reduce =
            let pred = Quatations.compileQuatationFilter filter
            items
            |> Array.Parallel.choose (fun x ->
                if pred x then 
                    Some (map x)
                else 
                    None
            )
            |> reduce
        { new IStore<'T> with
            member this.Report filter map reduce = report filter map reduce }
