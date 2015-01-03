namespace FSharp.BonsaiStore.Benchmarks
module SalesItemsBenchmarks =
    open System
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open SalesItems

    module SB = FSharp.BonsaiStore.StoreBuilder
    module R = FSharp.BonsaiStore.Reporting
    module U = FSharp.BonsaiStore.Benchmarks.Utils
    module T = Table

    /// Count using map reduce
    let totalSales(store: IStore<SalesItem>) (filter: Expr<SalesItem-> bool>) =
        R.report store filter (fun si -> si.Price) Array.sum<float>

    /// Count using map reduce
    let salesPerEmployee (store: IStore<SalesItem>) (filter: Expr<SalesItem-> bool>) =
        R.report
            store
            filter
            (fun si -> T.single si.Employee.EmployeeId si.Price) 
            (T.mergeWith (+))

    /// Count items
    let benchmark () =
        let numItems = int 1e4

        printfn "Generate %A sales items" numItems
        let items, m = U.memory <| fun () ->
            generateSalesItems numItems
        printfn "Size of items array: %A" m

        printfn "Build Store"
        let store, m  = U.memory <| fun _ -> 
            SB.buildDefaultStore items
        printfn "Size of store %A" m

        printfn "Build Cached Store"
        let storeC, m = U.memory <| fun _ ->
                let conf = {SB.BuildStoreConfiguration.Default with UseCaching = true}
                SB.buildStore conf items
        printfn "Size of cached store %A" m

        printfn "Build Array Store"
        let storeA , m = U.memory <| fun _ ->
            ArrayStore.buildStore items
        printfn "Size of array store %A" m

        let oneDayFilter = 
            <@ 
                fun (si: SalesItem) -> 
                    (si.Date = new DateTime(2010,1,1) )
                    && si.Price > 100. && si.Price < 200.
            @>
        let oneMonthFilter =
            <@ fun (si: SalesItem) -> 
                si.Date >= new DateTime(2010,1,1) && si.Date <= new DateTime(2010,2,1) 
                && si.Price > 100. && si.Price < 200.
            @>
        let oneYearFilter = 
            <@ fun (si: SalesItem) -> 
                si.Date >= new DateTime(2010,1,1) && si.Date <= new DateTime(2011,1,1) 
                && si.Price > 100. && si.Price < 200.
            @>

        let allTrue = 
            <@ fun (si: SalesItem) ->  true @>

        
        let totalSales filter =
            [
                "Store", Utils.testCase <| fun _    -> totalSales store filter
                "Cached", Utils.testCase <| fun _   -> totalSales storeC filter
                "Array", Utils.testCase <| fun _    -> totalSales storeA filter
            ]

        let salesPerEmployee filter =
            [
                "Store", Utils.testCase <| fun _ -> salesPerEmployee store filter
                "Cached", Utils.testCase <| fun _ -> salesPerEmployee storeC filter
                "Array", Utils.testCase <| fun _ -> salesPerEmployee storeA filter
            ]
        [
            "Total Sales One Day", totalSales oneDayFilter
            "Total Sales One Month", totalSales oneMonthFilter
            "Total Sales One Year", totalSales oneYearFilter
            "Total Sales All Periods", totalSales allTrue
//            "Sales Per Employee - One Day", salesPerEmployee oneDayFilter
//            "Sales Per Employee - One Month", salesPerEmployee oneMonthFilter
//            "Sales Per Employee - One Year", salesPerEmployee oneYearFilter
//            "Sales Per Employee - All Periods", salesPerEmployee allTrue
        ]
        |> Utils.benchmark 50