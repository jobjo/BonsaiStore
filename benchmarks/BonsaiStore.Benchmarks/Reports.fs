namespace FSharp.BonsaiStore.Benchmarks
module Reports =
    open Microsoft.FSharp.Quotations
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation
    
    open Date
    
    module SB = FSharp.BonsaiStore.StoreBuilder
    module U = FSharp.BonsaiStore.Benchmarks.Utils
    

    let toPredicate (exp: Expr<Date -> bool>) = exp.Compile() ()

    /// Count using map reduce
    let countStoreMapReduce (store: IBonsaiStore<Date, int>) (filter: Expr<Date -> bool>) =
        SB.report store filter 0  (fun _ -> 1) Array.length

    /// Count number of elements folding an array.
    let countArrayMapReduce dates (filter: Expr<Date -> bool>) =
        let pred = toPredicate filter
        (0, dates)
        ||> Array.fold (fun c d ->
            1 + if pred d then 1 else 0
        )

    /// Count items
    let countItemsBenchMark () =

        let numItems = int <| 1e7

        printfn "Generate %A dates" numItems
        let dates = Date.genDates numItems
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build store"
        let store : IBonsaiStore<Date.Date, int> = SB.buildStore dates
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Run benchmarks"
        let res =
            let n = int 1e6
            let oneYearFilter = <@ fun (date: Date) -> date.Year = 2010  @>
            let oneMonthFilter = <@ fun (date: Date) -> date.Month = 12 @>
            let oneDayFilter = <@ fun (date: Date) -> date.Year = 2010 && date.Month = 12 && date.Day = 3 @>
            let allFilter = <@ fun (date: Date) -> true @>
            [

                // Count one day
                "Store - Count one day", Utils.testCase <| fun _ -> countStoreMapReduce store oneDayFilter
                "Array - Count one day", Utils.testCase <| fun _ -> countArrayMapReduce dates oneDayFilter

                // Count one month
                "Store - Count one month", Utils.testCase <| fun _ -> countStoreMapReduce store oneMonthFilter
                "Array - Count one month", Utils.testCase <| fun _ -> countArrayMapReduce dates oneMonthFilter

                // Count one month
                "Store - Count one year", Utils.testCase <| fun _ -> countStoreMapReduce store oneYearFilter
                "Array - Count one year", Utils.testCase <| fun _ -> countArrayMapReduce dates oneYearFilter

                // Count all
                "Store - Count all", Utils.testCase <| fun _ -> countStoreMapReduce store allFilter
                "Array - Count all", Utils.testCase <| fun _ -> countArrayMapReduce dates allFilter

            ]
            |> Utils.benchmark 10
        for (n,t) in res do
            printfn "%s - %A" n t