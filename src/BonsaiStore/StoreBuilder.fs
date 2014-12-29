namespace FSharp.BonsaiStore
module StoreBuilder =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation

    module T = FSharp.BonsaiStore.Internal.Tree
    module F = FSharp.BonsaiStore.Internal.Filter
    module Q = FSharp.BonsaiStore.Quatations

    /// Builds a store
    let buildStore<'T> (items: seq<'T>) : IStore<'T> =

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
        let toFilter (exp: Expr<'T -> bool>) = 
            Q.buildFilterGenerator<'T> (List.map snd indexes) exp

        // Build the store
        let tree = T.buildTree T.defaultBuildTreeConfiguration levels items

        // Report function
        let report (filterExp: Expr<'T -> bool>) =
            let filterFun = Q.compileQuatationFilter filterExp
            let filter = toFilter filterExp
            fun (mr: MapReducer<'T,'R>) ->
                let map =
                    Array.Parallel.choose (fun x ->
                        if filterFun x then Some (mr.Map x) else None
                    )
                    >> mr.Reduce
                F.report mr.Empty map mr.Reduce filter tree
        { new IStore<'T> with member this.Report exp mr = report exp mr }