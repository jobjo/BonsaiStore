namespace FSharp.BonsaiStore.Tests

module Common =

    open FSharp.BonsaiStore.Internal.Filter
    module T = FSharp.BonsaiStore.Internal.Tree

    /// Simple date type
    type Date = int * int * int

    /// Level
    type DateProperty =
        | Year
        | Month
        | Day
        override this.ToString() =
            match this with
            | Year  -> "Year"
            | Month -> "Month"
            | Day   -> "Day"

    /// Build a tranche tree.
    let buildDateTree xs =
        let levels  =
            [
                Year, fun (y,_,_) -> y
                Month, fun (_,m,_) -> m
                Day, fun (_,_,d) -> d
            ]
        let conf = T.defaultBuildTreeConfiguration
        T.buildTree conf levels xs

    /// Tree of dates
    let dateTree =
        [
            for y in [2000 .. 2010] do
                for m in [1..12] do
                    for d in [1..28] do
                        yield y,m,d
        ]
        |> buildDateTree

    /// Property helpers
    let year x = equal Year x
    let month x = equal Month x
    let day x = equal Day x

    /// Predicate builder
    let toPredicate x =
        let f =
            toPredicate <| fun ft  (y,m,d) ->
                match ft with
                | Year  -> y
                | Month -> m
                | Day   -> d
        f x


    /// Filter by 
    let filterByPredicate f =
        T.elements
        >> Array.concat  
        >> Array.filter (toPredicate f)
        >> Array.sort