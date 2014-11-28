namespace FSharp.BonsaiStore.Examples

module SalesItemGenerator =
    open System
    open FsCheck
    open Microsoft.FSharp.Quotations

    open SalesItem

    let private salesItemGen =
        let toolsProduct =
            [
                10, Gen.constant ("Screwdriver", 50.)
                5, Gen.constant ("Hammer", 50.)
                5, Gen.constant ("Nails", 10.)
                3, Gen.constant ("Wrench", 100.)
                1, Gen.constant ("Chainsaw", 1000.)
            ]
            |> Gen.frequency
            |> Gen.map (fun (name,price) -> {Category ="Tools"; Name = name }, price)
        
        let electronicsProduct =
            [
                30, Gen.constant ("USB drive", 40.)
                15, Gen.constant ("Hard Drive", 500.)
                10, Gen.constant ("Laptop", 5000.)
            ]
            |> Gen.frequency
            |> Gen.map (fun (name,price) -> {Category ="Electronics"; Name = name }, price)
        
        let musicProduct =
            [
                10, Gen.constant ("Pan Fluit", 100.)
                8, Gen.constant ("Guitar", 400.)
                3, Gen.constant ("Drum Set", 1000.)
            ]
            |> Gen.frequency
            |> Gen.map (fun (name,price) -> {Category ="Music"; Name = name }, price)

        let product =
            [
                1, musicProduct
                2, toolsProduct
                5, electronicsProduct
            ]   
            |> Gen.frequency
              
        let quantity =
            [
                10, Gen.constant 1
                5, Gen.constant 2
                1, Gen.choose (1,10)
            ]       
            |> Gen.frequency

        let date =
            gen {
                let! y = Gen.choose (2010, 2013)
                let! m = 
                    [
                       3, Gen.choose (1,4)
                       2, Gen.choose (5,8)
                       3, Gen.choose (8,12) 
                    ]
                    |> Gen.frequency
                let! d = Gen.choose(1,28)
                return DateTime(y,m,d)
            }
        
        let lisa = {Name = "Lisa Anderson"; Team = "A-Team"}
        let paul = {Name = "Paul Smith"; Team = "A-Team"}
        let sara = {Name = "Sara Smith"; Team = "A-Team"}
        let lars = {Name = "Lars Ulrik"; Team = "B-Team"}
        let pia = {Name = "Pia Jones"; Team = "B-Team"}
        let mike = {Name = "Michael Green"; Team = "C-Team"}
        
        let person cat =
            match cat with
            | "Tools"   -> 
                [
                    1, Gen.constant lisa
                    2, Gen.constant paul
                    1, Gen.constant sara
                ]
            | "Electronics" ->
                [
                    1, Gen.constant lisa
                    1, Gen.constant lars
                    5, Gen.constant pia
                ]
            | _             ->
                [1, Gen.constant mike]
            |> Gen.frequency

        gen {
            let! prod,price = product
            let! person = person prod.Category
            let! date = date
            let! quant = quantity
            let price = price * (float quant)
            return
                {
                    Product = prod
                    Quantity = quant
                    Price = price
                    Date = date
                    Person = person
                }
        }

    /// Generate n random sales items.
    let randomSalesItems n =
        let seed = ref <|  Random.newSeed ()
        [
            for i in [1 .. n] do
                let (_,s) = Random.stdNext seed.Value
                seed := s
                yield Gen.eval 10 seed.Value salesItemGen
        ]