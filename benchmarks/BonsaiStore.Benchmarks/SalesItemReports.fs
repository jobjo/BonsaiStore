namespace FSharp.BonsaiStore.Benchmarks
module SalesItemsReports =
    open System
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open SalesItems

    module SB = FSharp.BonsaiStore.StoreBuilder
    module R = FSharp.BonsaiStore.Reporting
    module U = FSharp.BonsaiStore.Benchmarks.Utils
    module T = Table

    /// Count using map reduce
    let totSalesStoreMapReduce(store: IStore<SalesItem>) (filter: Expr<SalesItem-> bool>) =
        R.report store filter (fun si -> si.Price) Array.sum<float>

    /// Count using map reduce
    let salesPerEmployeeStoreMapReduce (store: IStore<SalesItem>) 
                                       (filter: Expr<SalesItem-> bool>) =
        R.report
            store
            filter
            (fun si -> T.single si.Employee.EmployeeId si.Price) 
            (T.mergeWith (+))

    /// Count items
    let totalSalesBenchMark () =
        let numItems = int 1e6

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

        
//        let r1 = salesPerEmployeeStoreMapReduce storeA oneDayFilter
//        let r2 = salesPerEmployeeStoreMapReduce store oneDayFilter
//        let r3 = salesPerEmployeeStoreMapReduce storeC oneDayFilter
//
//        printfn "========================="
//        T.showTable r1
//        printfn "========================="
//        T.showTable r2
//        printfn "========================="
//        T.showTable r3
        
        let totalSales filter =
            [
                "Store", Utils.testCase <| fun _    -> totSalesStoreMapReduce store filter
                "Cached", Utils.testCase <| fun _   -> totSalesStoreMapReduce storeC filter
                "Array", Utils.testCase <| fun _    -> totSalesStoreMapReduce storeA filter
            ]

        let salesPerEmployee filter =
            [
                "Store", Utils.testCase <| fun _ -> salesPerEmployeeStoreMapReduce store filter
                "Cached", Utils.testCase <| fun _ -> salesPerEmployeeStoreMapReduce storeC filter
                "Array", Utils.testCase <| fun _ -> salesPerEmployeeStoreMapReduce storeA filter
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