namespace FSharp.BonsaiStore.Benchmarks
module SalesItemsReports =
    open System
    open Microsoft.FSharp.Quotations
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open SalesItems

    module SB = FSharp.BonsaiStore.StoreBuilder
    module U = FSharp.BonsaiStore.Benchmarks.Utils

    let toPredicate (exp: Expr<SalesItem -> bool>) = exp.Compile() ()

    /// Count using map reduce
    let totSalesStoreMapReduce (store: IBonsaiStore<SalesItem>) (filter: Expr<SalesItem-> bool>) =
        SB.report store filter 0.  (fun si -> si.Price) Array.sum

    /// Count using map reduce
    let totSalesStoreMapReduceParallel (store: IBonsaiStore<SalesItem>) (filter: Expr<SalesItem-> bool>) =
        SB.reportParallel store filter 0.  (fun si -> si.Price) Array.sum

    /// Count using map reduce
    let totSalesStoreArray (items: SalesItem []) (filter: Expr<SalesItem-> bool>) =
        let pred = toPredicate filter
        (0., items)
        ||> Array.fold (fun c si ->
            si.Price + if pred si then si.Price else 0.
        )

    /// Count items
    let totalSalesBenchMark () =
        let numItems = int 1e5

        printfn "Generate %A sales items" numItems
        let items = generateSalesItems numItems
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build store"
        let store = SB.buildStore items
        printfn "Memory %A" (Utils.getCurrentMemory())

        let oneDayFilter = <@ fun (si: SalesItem) -> si.Date = new DateTime(2010,12,25) @>
        let oneMonthFilter =
            <@ fun (si: SalesItem) -> 
                si.Date >= new DateTime(2010,1,1) && si.Date <= new DateTime(2011,2,1) 
            @>
        let oneYearFilter = 
            <@ fun (si: SalesItem) -> 
                si.Date >= new DateTime(2010,1,1) && si.Date <= new DateTime(2011,1,1) 
            @>

        let allTrue = 
            <@ fun (si: SalesItem) ->  true @>

        let test filter =
            [
                "Store", Utils.testCase <| fun _ -> totSalesStoreMapReduce store filter
                "Store (P)", Utils.testCase <| fun _ -> totSalesStoreMapReduceParallel store filter
                "Array", Utils.testCase <| fun _ -> totSalesStoreArray items filter
            ]
        [
            "Total Sales One Day", test oneDayFilter
            "Total Sales One Month", test oneMonthFilter
            "Total Sales One Year", test oneYearFilter
            "All", test allTrue

        ]
        |> Utils.benchmark 10