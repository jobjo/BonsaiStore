namespace FSharp.BonsaiStore.Benchmarks
module Reports =
    open FSharp.BonsaiStore
    module SB = FSharp.BonsaiStore.StoreBuilder
    module U = FSharp.BonsaiStore.Benchmarks.Utils

    /// Count the number of items by filtering store.
    let countNumStore (store: IBonsaiStore<Date.Date, int>) (y:int) (m:int) (d:int) =
        let s = sprintf "%A %A %A" y m d
        SB.filter store 
            <@ fun date -> 
                date.DisplayString = s
            @>
        |> Seq.length

    /// Count number of items by manually filtering array.
    let countNumArr dates y m d =
        let s = sprintf "%A %A %A" y m d
        dates
        |> Seq.filter (fun (date: Date.Date) -> date.DisplayString = s)
        |> Seq.length

    /// Count items
    let countItemsBenchMark () =

        let numItems = int <| 1e6

        printfn "Generate %A dates" numItems
        let dates = Date.genDates numItems
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Build store"
        let store : IBonsaiStore<Date.Date, int> = SB.buildStore dates
        printfn "Memory %A" (Utils.getCurrentMemory())

        printfn "Run benchmarks"
        let res =
            [
                "Array", Utils.testCase <| fun _ -> countNumArr dates 2010 10 12
                "Store", Utils.testCase <| fun _ -> countNumStore store 2010 10 12
            ]
            |> Utils.benchmark 100
        for (n,t) in res do
            printfn "%s - %A" n t