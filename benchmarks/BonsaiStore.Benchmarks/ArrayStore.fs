namespace FSharp.BonsaiStore.Benchmarks
module ArrayStore =
    open FSharp.BonsaiStore
    let buildStore items =
        let report choose filter (mr: MapReducer<_,_>) =
            let pred = Quatations.compileQuatationFilter filter
            items
            |> choose (fun x ->
                if pred x then 
                    Some (mr.Map x)
                else 
                    None
            )
            |> mr.Reduce
        { new IStore<'T> with
            member this.Report exp mr = report Array.choose exp mr
        }
