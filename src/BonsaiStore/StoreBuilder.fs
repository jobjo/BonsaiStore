namespace FSharp.BonsaiStore
module StoreBuilder =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation

    module T = FSharp.BonsaiStore.Internal.Tree
    module F = FSharp.BonsaiStore.Internal.Filter
    module Q = FSharp.BonsaiStore.Quatations

    /// Builds a store
    let buildStore<'T> (items: seq<'T>) : IBonsaiStore<'T> =

        // Find indexes from type.
        let indexes = Q.extractIndexes<'T>()

        // Get levels.
        let levels =
            indexes
            |> List.sortBy fst
            |> List.mapi (fun i (l, exp) ->
                let exp = Expr.Cast(exp) : Expr<'T -> int>
                (i,  exp.Compile()())
            )

        // To filter function
        let toFilter = Q.buildFilterGenerator<'T> (List.map snd indexes)

        // Build the store
        let tree = T.buildTree T.defaultBuildTreeConfiguration levels items

        // Report function
        let report (filterExp: Expr<'T -> bool>) =
            let filterFun = filterExp.Compile() ()
            let filter = toFilter filterExp
            printfn "%s" (F.showFilter filter)
            printfn "========================================"
            fun (mr: MapReducer<'T,'R>) ->
                let map xs =
                    let ys =
                        xs
                        |> Array.choose (fun x ->
                            if filterFun x then Some (mr.Map x) else None
                        )
                    mr.Reduce ys
                F.mapReduce mr.Empty map mr.Reduce filter tree

        // Report function
        let reportParallel (filterExp: Expr<'T -> bool>) =
            let filterFun = filterExp.Compile() ()
            let filter = toFilter filterExp
            fun (mr: MapReducer<'T,'R>) ->
                let map xs =
                    let ys =
                        xs
                        |> Array.choose (fun x ->
                            if filterFun x then Some (mr.Map x) else None
                        )
                    mr.Reduce ys
                F.mapReduceParallel mr.Empty map mr.Reduce filter tree

        // Report function
        let filter (filterExp: Expr<'T -> bool>) =
            let filterFun = filterExp.Compile() ()
            let filter = toFilter filterExp
            let res = F.find filter tree
            Seq.collect (Seq.filter filterFun) res

        { new IBonsaiStore<'T> with
            member this.Report exp mr = report exp mr
            member this.Filter exp = filter exp
            member this.ReportParallel exp mr = reportParallel exp mr
            member this.Add x = failwith "Not implemented"
        }

    /// Filter
    let filter<'T,'K when 'K: comparison>(store: IBonsaiStore<'T>) =
        store.Filter

    /// Create a report from a store.
    let report<'T,'R>  (store: IBonsaiStore<'T>) 
                       (filter: Expr<'T -> bool>) 
                       (empty: 'R) 
                       (map: 'T -> 'R) 
                       (reduce: 'R [] -> 'R) 
                       : 'R =
        store.Report filter { Empty = empty; Map = map; Reduce = reduce}

    /// Create a report from a store.
    let reportParallel<'T,'R>  (store: IBonsaiStore<'T>) 
                               (filter: Expr<'T -> bool>) 
                               (empty: 'R) 
                               (map: 'T -> 'R) 
                               (reduce: 'R [] -> 'R) 
                               : 'R =
        store.ReportParallel filter { Empty = empty; Map = map; Reduce = reduce}