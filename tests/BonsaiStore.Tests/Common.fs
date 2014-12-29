namespace FSharp.BonsaiStore.Tests

module Common =

    open FSharp.BonsaiStore.Internal.Filter
    module T = FSharp.BonsaiStore.Internal.Tree

    /// Simple date type
    type Date = int * int * int


    /// Build a tranche tree.
    let buildDateTree xs =
        let levels  =
            [
                0, fun (y,_,_) -> y
                1, fun (_,m,_) -> m
                2, fun (_,_,d) -> d
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
    let year x = equal 0 x
    let month x = equal 1 x
    let day x = equal 2 x

    /// Predicate builder
    let toPredicate x =
        let f =
            toPredicate <| fun ft  (y,m,d) ->
                match ft with
                | 0 -> y
                | 1 -> m
                | _ -> d
        f x


    /// Filter by 
    let filterByPredicate f =
        T.elements
        >> Array.filter (toPredicate f)
        >> Array.sort