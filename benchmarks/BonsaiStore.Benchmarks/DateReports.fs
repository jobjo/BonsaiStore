namespace FSharp.BonsaiStore.Benchmarks
module DateReports =
    open Microsoft.FSharp.Quotations
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation
    
    open Date
    
    module SB = FSharp.BonsaiStore.StoreBuilder
    module U = FSharp.BonsaiStore.Benchmarks.Utils

    let toPredicate (exp: Expr<Date -> bool>) = exp.Compile() ()

    /// Count using map reduce
    let countStoreMapReduce (store: IBonsaiStore<Date>) (filter: Expr<Date -> bool>) =
        SB.report store filter 0  (fun _ -> 1) Array.length

    /// Count using map reduce
    let countStoreMapReduceParallel (store: IBonsaiStore<Date>) (filter: Expr<Date -> bool>) =
        SB.reportParallel store filter 0  (fun _ -> 1) Array.length

    /// Count number of elements folding an array.
    let countArrayMapReduce dates (filter: Expr<Date -> bool>) =
        let pred = toPredicate filter
        (0, dates)
        ||> Array.fold (fun c d ->
            c  + if pred d then 1 else 0
        )

    /// Count items
    let countItemsBenchMark () =

        let numItems = int <| 1e5

        printfn "Generate %A dates" numItems
        let dates = Date.genDates numItems
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build store"
        let store : IBonsaiStore<Date.Date> = SB.buildStore dates
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Run benchmarks"
        let oneYearFilter = <@ fun (date: Date) -> date.Year = 2010  @>
        let oneMonthFilter = <@ fun (date: Date) -> date.Month = 12 @>
        let oneDayFilter = <@ fun (date: Date) -> date.Year = 2010 && date.Month = 12 && date.Day = 3 @>
        let allFilter = <@ fun (date: Date) -> true @>
        let dispStringFilter = <@ fun (date: Date) -> date.DisplayString = "2010-07-12" @>
        [
            
            "Count Display Index for one day",
                [
                    "Store", Utils.testCase <| fun _ -> countStoreMapReduce store dispStringFilter
                    "Store (P)", Utils.testCase <| fun _ -> countStoreMapReduceParallel store dispStringFilter
                    "Array", Utils.testCase <| fun _ -> countArrayMapReduce dates dispStringFilter
                ]
            
            "Count One day", 
                [
                    "Store", Utils.testCase <| fun _ -> countStoreMapReduce store oneDayFilter
                    "Store (P)", Utils.testCase <| fun _ -> countStoreMapReduceParallel store oneDayFilter
                    "Array", Utils.testCase <| fun _ -> countArrayMapReduce dates oneDayFilter
                ]
            
            "Count one month", 
                [
                    "Store", Utils.testCase <| fun _ -> countStoreMapReduce store oneMonthFilter
                    "Store (P)", Utils.testCase <| fun _ -> countStoreMapReduceParallel store oneMonthFilter
                    "Array", Utils.testCase <| fun _ -> countArrayMapReduce dates oneMonthFilter
                ]

            "Count one year", 
                [
                    "Store", Utils.testCase <| fun _ -> countStoreMapReduce store oneYearFilter
                    "Store (P)", Utils.testCase <| fun _ -> countStoreMapReduceParallel store oneYearFilter
                    "Array", Utils.testCase <| fun _ -> countArrayMapReduce dates oneYearFilter
                ]

            "Count all", 
                [
                    "Store", Utils.testCase <| fun _ -> countStoreMapReduce store allFilter
                    "Store (P)", Utils.testCase <| fun _ -> countStoreMapReduceParallel store allFilter
                    "Array", Utils.testCase <| fun _ -> countArrayMapReduce dates allFilter
                ]
        ]
        |> Utils.benchmark 10