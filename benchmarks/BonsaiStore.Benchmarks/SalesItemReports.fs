namespace FSharp.BonsaiStore.Benchmarks
module SalesItemsReports =
    open System
    open Microsoft.FSharp.Quotations
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open SalesItems

    module SB = FSharp.BonsaiStore.StoreBuilder
    module R = FSharp.BonsaiStore.Reporting
    module U = FSharp.BonsaiStore.Benchmarks.Utils
    module T = Table

    /// Count using map reduce
    let totSalesStoreMapReduce(store: IStore<SalesItem>) (filter: Expr<SalesItem-> bool>) =
        R.report store filter 0. (fun si -> si.Price) Array.sum<float>

    /// Count using map reduce
    let salesPerEmployeeStoreMapReduce (store: IStore<SalesItem>) 
                                       (filter: Expr<SalesItem-> bool>) =
        R.report
            store
            filter
            T.empty
            (fun si -> T.single si.Employee.EmployeeId si.Price) 
            (T.mergeWith (+))

    /// Count items
    let totalSalesBenchMark () =
        let numItems = int 1e6

        printfn "Generate %A sales items" numItems
        let items = generateSalesItems numItems
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build store"
        let store = SB.buildStore items
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build Array Store"
        let storeA = ArrayStore.buildStore items
        printfn "Memory %A" (Utils.getCurrentMemory())

        let oneDayFilter = 
            <@ 
                fun (si: SalesItem) -> (si.Date = new DateTime(2010,1,1) )
            @>
        let oneMonthFilter =
            <@ fun (si: SalesItem) -> 
                si.Date >= new DateTime(2010,1,1) && si.Date <= new DateTime(2010,2,1) 
            @>
        let oneYearFilter = 
            <@ fun (si: SalesItem) -> 
                si.Date >= new DateTime(2010,1,1) && si.Date <= new DateTime(2011,1,1) 
            @>

        let allTrue = 
            <@ fun (si: SalesItem) ->  true @>

        let totalSales filter =
            [
                "Store", Utils.testCase <| fun _ -> totSalesStoreMapReduce store filter
                "Array", Utils.testCase <| fun _ -> totSalesStoreMapReduce storeA filter
            ]

        let salesPerEmployee filter =
            [
                "Store", Utils.testCase <| fun _ -> salesPerEmployeeStoreMapReduce store filter
                "Array", Utils.testCase <| fun _ -> salesPerEmployeeStoreMapReduce storeA filter
            ]
        [
//            "Total Sales One Day", totalSales oneDayFilter
//            "Total Sales One Month", totalSales oneMonthFilter
//            "Total Sales One Year", totalSales oneYearFilter
//            "Total Sales All Periods", totalSales allTrue
            "Sales Per Employee - One Day", salesPerEmployee oneDayFilter
            "Sales Per Employee - One Month", salesPerEmployee oneMonthFilter
            "Sales Per Employee - One Year", salesPerEmployee oneYearFilter
            "Sales Per Employee - All Periods", salesPerEmployee allTrue
        ]
        |> Utils.benchmark 10