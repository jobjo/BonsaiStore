namespace FSharp.BonsaiStore.Benchmarks
module SalesItems =
    open FSharp.BonsaiStore
    open System
    module SB = FSharp.BonsaiStore.StoreBuilder

    type Employee = { EmployeeId : int ; Name : string; Team : string }
    type Product = { ProductId : int; Category : string ; Name : string }
    type SalesItem = 
        {
            Product : Product
            Quantity : int
            Price : float
            Date : System.DateTime
            Employee : Employee 
        }
        [<Index; Level(0)>]
        member private this.DateIx() = 
            let span = this.Date - DateTime(1900,1,1)
            span.Days / 100

        [<Index; Level(1)>]
        member private this.PriceIx() = 
            int (this.Price / 300.)

        [<Index; Level(2)>]
        member private this.PersonNameIx() = 
            this.Employee.Name.GetHashCode() % 100

        [<Index; Level(3)>]
        member private this.CategoryIx() = 
            this.Product.Category.ToString().GetHashCode()

//        [<Index>]
//        member private this.TeamIx() = 
//            abs (this.Employee.Team.GetHashCode()) % 5
//
//        [<Index>]
//        member private this.QuantityIx() = 
//            this.Quantity % 10
//
//        [<Index>]
//        member private this.EmployeedIdIx() = 
//            this.Employee.EmployeeId % 10
    
    let generateSalesItems n =
        let r = new Random()
        let genName =
            let fNames = [|"Abdul"; "Andras"; "Anna"; "Bo"; "Cynthia"; "Jennifer"; "Gabor"; "John"; "Michele"; "Atilla"; "Balazs"; "Johny"; "Nora"; |]
            let lNames = [|"Abramsson"; "Johnson"; "Williams"; "Pearson"; "Smith"; "Davis"; "Georgia"; "White"; "Black"; "Miller"; "ONeal"; "Stockton"|]
            fun () ->
                let fName = fNames.[r.Next() % fNames.Length]
                let lName = lNames.[r.Next() % lNames.Length]
                fName + " " + lName
        let genTeam =
            let names = [| "Olympos"; "Team Berlin"; "A-Team"; "Z-Team"; "Team France"; "Rocket Builders"; |]
            fun () ->
                names.[r.Next() % names.Length]
        let genCategory =
            let cats = [| "Kitchen"; "Outdoor"; "All Around"; "Premium"; |]
            fun () -> cats.[r.Next() % cats.Length]
        let genProductName =
            let types = [| "Knot"; "Bolt"; "Grip"; "Tape"; "Sink"; "Chain"; "Ring" ; "Washer" |]
            let fs = [| "Space"; "Smurf"; "Alpha"; "Helium"; "Rubber" ; "Metal" ; "Party"|]
            let ats = [| "Big"; "Thin" ; "Thick" ; "Small" ; "XXL" ; "M"; "S"; "L" |]
            let codes = [| "SKO"; "FXL"; "VPK"; "KD"; "M"; "SD" |]
            fun () ->
                sprintf "%s %s %s %s" 
                    ats.[r.Next() % ats.Length]
                    fs.[r.Next() % fs.Length]
                    types.[r.Next() % types.Length]
                    codes.[r.Next() % codes.Length]
        let employees =
            [
                for i in [1 .. 1000] do
                    let name = genName()
                    let team = genTeam ()
                    yield {EmployeeId = i; Name = name; Team = team}
            ]
        let products = 
            [
                for i in [1 .. 1000] do
                    yield { ProductId = i; Category = genCategory(); Name = genProductName()}
            ]
        let dates =
            let d0 = DateTime(2000, 1, 1)
            [for d in [1 .. 365 * 15] do yield d0.AddDays(float d)]
        [|
            for i in [1 .. n] do
                let prod =  products.[r.Next() % products.Length]
                let qnt = 1 + r.Next() % 10
                let price = float <| prod.Name.Length * 10 * qnt
                yield {
                    Product = prod
                    Quantity = qnt
                    Price = price
                    Date = dates.[r.Next() % dates.Length]
                    Employee = employees.[r.Next() % employees.Length]
                }
        |]

