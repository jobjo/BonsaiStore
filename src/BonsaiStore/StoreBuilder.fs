namespace FSharp.BonsaiStore
module internal StoreBuilder =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation
    
    module T = FSharp.BonsaiStore.Internal.Tree
    module F = FSharp.BonsaiStore.Internal.Filter
    module Q = FSharp.BonsaiStore.Quotations


    /// Create tree configuration.
    let private buildTreeConf conf =
        let df = T.defaultBuildTreeConfiguration
        { new T.IBuildTreeConfiguration with 
            member this.CacheElements() = conf.UseCaching
            member this.BuildMap<'K,'V when 'K : comparison> (xs: seq<'K * 'V>)  =
                df.BuildMap xs
        }


    /// Builds a store.
    let buildStore<'T> (conf: BuildStoreConfiguration) (items: seq<'T>) : IStore<'T> =
        let treeConf = buildTreeConf conf
        
        // Find indexes from type.
        let indexes = Q.extractIndexes<'T>()

        // To filter function
        let toFilter (exp: Expr<'T -> bool>) = 
            Q.buildFilterGenerator<'T> indexes exp

        // Get levels.
        let levels =
            indexes
            |> List.sortBy (fun info -> info.Level)
            |> List.mapi (fun i info ->
                (i,  info.Expression.Compile()())
            )

        // Builds a store given an initial seed.
        let rec build tree =

            // Report function
            let report (filterExp: Expr<'T -> bool>) =
                let filterFun = Q.compileQuotationFilter filterExp
                let filter = toFilter filterExp
                fun (map: 'T -> 'R) (reduce: 'R [] -> 'R) ->
                    let empty = reduce [||]
                    let map x = if filterFun x then map x else empty
                    F.report tree filter map reduce

            // Insert a sequence of items.
            let insert = T.insert treeConf levels tree >> build

            // Filters the tree.
            let filter pred = build <| T.filter pred tree 

            // Map over elements.
            let map f = build <| T.map f tree

            { new IStore<'T> with
                member this.Report filter map reduce = report filter map reduce 
                member this.Insert items = insert items
                member this.Filter pred = filter pred
                member this.Map f = map f
            }

        // Build the store
        build <| T.buildTree treeConf levels items

    /// Builds a store
    let buildDefaultStore<'T> (items: seq<'T>) : IStore<'T> =
        buildStore BuildStoreConfiguration.Default items