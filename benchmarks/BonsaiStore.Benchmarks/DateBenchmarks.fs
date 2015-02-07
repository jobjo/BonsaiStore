namespace FSharp.BonsaiStore.Benchmarks
module DateBenchmarks =
    open FSharp.BonsaiStore
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open Date

    module U = FSharp.BonsaiStore.Benchmarks.Utils

    let toPredicate (exp: Expr<Date -> bool>) = exp.Compile() ()

    /// Count using map reduce
    let count (store: IStore<Date>) (filter: Expr<Date -> bool>) =
        report store filter (fun _ -> 1) Array.length

    /// Count items
    let countItemsBenchMark () =

        let numItems = int <| 1e6

        printfn "Generate %A dates" numItems
        let dates = Date.genDates numItems
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build store"
        let store : IStore<Date.Date> = buildDefaultStore dates
        let storeA = ArrayStore.buildStore dates

        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Run benchmarks"
        let oneYearFilter = <@ fun (date: Date) -> date.Year = 2010  @>
        let oneMonthFilter = <@ fun (date: Date) -> date.Month = 12 @>
        let oneDayFilter = <@ fun (date: Date) -> date.Year = 2010 && date.Month = 12 && date.Day = 3 @>
        let allFilter = <@ fun (date: Date) -> true @>
        let dispStringFilter = <@ fun (date: Date) -> date.DisplayString = "2010-07-12" @>
        
        let count title filter=
            title,
                [
                    "Store", Utils.testCase <| fun _ -> count store filter
                    "Array", Utils.testCase <| fun _ -> count storeA filter
                ]
        [
            
            count "Count Display Index for one day" dispStringFilter
            count "Count One day" oneDayFilter
            count "Count one month" oneMonthFilter
            count "Count one year" oneYearFilter
            count "Count all" allFilter
        ]
        |> Utils.benchmark 10