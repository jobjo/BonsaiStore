namespace FSharp.BonsaiStore.Examples

module InternetSalesStore =
    open System
    open FSharp.BonsaiStore.Examples.AdventureWorks
    open FSharp.BonsaiStore
    module SB = FSharp.BonsaiStore.StoreBuilder

    // Load all internet sales items and builds the store
    let buildSalesStore () : IBonsaiStore<InternetSales, int> =
        let iss = DataLoader.readInternetSalesItems()
        let iss = List.replicate 10 iss |> List.collect id
        SB.buildStore iss
