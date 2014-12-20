namespace FSharp.BonsaiStore.Benchmarks
module Date =
    open FSharp.BonsaiStore
    module SB = FSharp.BonsaiStore.StoreBuilder

    type Date =
        {
            Year : int
            Month : int
            Day : int
            DisplayString : string
        }

        [<Index; Level(0) >]
        member private this.DisplayIx = this.DisplayString.GetHashCode()
        
        [<Index; Level(1)>]
        member private this.YearIx = this.Year

        [<Index; Level(2)>]
        member private this.MonthIx = this.Month

        [<Index; Level(3)>]
        member private this.DayIx = this.Day


    /// Builds a date store.
    let genDates n =
        let r = System.Random()
        let items =
            [
                for _ in [1 .. n] do
                    let y = 2000 + (r.Next() % 20)
                    let m = 1 + (r.Next() % 12)
                    let d = 1 + (r.Next() % 28)
                    let ds = y.ToString() + m.ToString() + d.ToString()
                    yield {Year = y; Month = m; Day = d; DisplayString = ds}
            ]
        Array.ofList items

