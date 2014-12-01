namespace FSharp.BonsaiStore.Examples

module SalesItem =

    open FSharp.BonsaiStore
    
    type Person =
        {
            Name : string
            Team : string
        }

    type Product =
        {
            Category : string
            Name : string
        }

    type SalesItem =
        {
            Product : Product
            Quantity : int
            Price : float
            Date : System.DateTime
            Person : Person
        }
        [<Index; Level(1)>]
        member private this.DayIx() = this.Date.Day
        
        [<Index; Level(2)>]
        member private this.MonthIx() = this.Date.Month
        
        [<Index; Level(3)>]
        member private this.YearIx() = this.Date.Year

        [<Index; Level(4)>]
        member private this.PersonNameIx() = this.Person.Name.GetHashCode()

        [<Index; Level(5)>]
        member private this.PriceIx() = int (this.Price / 100.)

        override this.ToString () =
            let date = sprintf "%A-%A-%A" this.Date.Year this.Date.Month this.Date.Day
            sprintf "%s %A %A %s %s" this.Product.Name this.Quantity this.Price date this.Person.Name