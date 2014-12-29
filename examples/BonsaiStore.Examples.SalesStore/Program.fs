namespace FSharp.BonsaiStore.Examples

module Program =

    open SalesItem
    module SB = FSharp.BonsaiStore.StoreBuilder
    module R = FSharp.BonsaiStore.Reporting
    module T = Table
    open FSharp.BonsaiStore

    /// Builds a sales item store with random items.
    let buildSalesItemStore () =
        printfn "Generating items"
        let salesItems = SalesItemGenerator.randomSalesItems (int 1e3)
        printfn "Building the store"
        let store = SB.buildDefaultStore<SalesItem> salesItems
        printfn "Done building the item store"
        store

    /// Top team report.
    let topTeams (salesStore: IStore<SalesItem>) filter =
        R.report
            salesStore
            filter
            T.empty
            (fun item -> T.fromSeq [item.Person.Team, float item.Quantity * item.Price]) 
            T.merge
    
    /// Count elements
    let totalNumSales (salesStore: IStore<SalesItem>) filter =
        R.report
            salesStore
            filter
            0
            (fun _ -> 1)
            Seq.sum

    [<EntryPoint>]
    let main argv = 
        printfn "Build store"
        let salesStore = buildSalesItemStore ()
        printfn "Done building store\n\n"

        // Total number of sales of items with price over 100 in 2010.
        let totalNumSalesRes =
            totalNumSales salesStore <@ fun item ->  item.Price > 100. && item.Date.Year = 2010 @>
        printfn "Number of sales with price over 100 2010: %A\n" totalNumSalesRes

        // Top teams July 2011
        let topTeamsRes =
            topTeams salesStore <@ fun item ->  
                item.Date.Year = 2011  && item.Date.Month = 7 @>
        printfn "Top teams July 2011:"
        printfn "--------------------"
        Table.showTable topTeamsRes
        0