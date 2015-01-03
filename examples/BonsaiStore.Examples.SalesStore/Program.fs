namespace FSharp.BonsaiStore.Examples

module Program =

    open SalesItems
    open FSharp.BonsaiStore
    module SB = FSharp.BonsaiStore.StoreBuilder
    module S = FSharp.BonsaiStore.Service.Store

    module T = Table


    /// Builds a sales item store with random items.
    let buildSalesItemStore () =
        let numItems = int 1e5
        printfn "Generating items"
        let salesItems = generateSalesItems numItems
        printfn "Building the store"
        let service = 
            SB.buildDefaultStore<SalesItem> salesItems
            |> S.buildService
        printfn "Done building the item store"
        service

    /// Top team report.
    let topTeams service filter =
        S.report
            service
            filter
            (fun item -> T.fromSeq [item.Employee.Team, float item.Quantity * item.Price])
            T.merge

    /// Count elements
    let totalNumSales service filter =
        S.report
            service
            filter
            (fun _ -> 1)
            Array.sum

    [<EntryPoint>]
    let main argv = 
        printfn "Build store"
        let salesStore = buildSalesItemStore ()
        printfn "Done building store\n\n"

        // Total number of sales of items with price over 100 in 2010.
        let totalNumSalesRes =
            totalNumSales salesStore 
                <@ fun item ->  
                    item.Price > 100. && item.Date.Year = 2010 
                @>
        printfn "Number of sales with price over 100 2010: %A\n" totalNumSalesRes

        // Top teams July 2011
        let topTeamsRes =
            topTeams salesStore 
                <@ fun item ->
                    item.Date.Year = 2011  && item.Date.Month = 7 
                @>

        printfn "Top teams July 2011:"
        printfn "--------------------"
        Table.showTable topTeamsRes
        0