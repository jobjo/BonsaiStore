namespace FSharp.BonsaiStore.Examples

module Program =
    open SalesItems

    open FSharp.BonsaiStore
    open FSharp.BonsaiStore.Service
    module T  = Reports.Table
    module N  = Reports.Numerical

    /// Builds a sales item store with random items.
    let buildSalesItemStore () =
        let numItems = int 1e5
        printfn "Generating items"
        let salesItems = generateSalesItems numItems
        printfn "Building the store"
        let service = buildDefaultService salesItems
        printfn "Done building the item store"
        service

    /// Top team report.
    let topTeams service filter =
        report
            service
            filter
            (fun item -> T.single item.Employee.Team  (float item.Quantity * item.Price))
            (T.mergeWith (+))

    /// Count elements
    let totalNumSales service filter =
        N.sumBy service filter (fun _ -> 1.)

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

        0