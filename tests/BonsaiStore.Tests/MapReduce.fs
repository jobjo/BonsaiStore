namespace FSharp.BonsaiStore.Tests

module MapReduce =
    open FsCheck.Xunit
    open Common

    module F = FSharp.BonsaiStore.Internal.Filter
    module T = FSharp.BonsaiStore.Internal.Tree
    

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Map-reduce same as filter, map and reduce`` (f: F.Filter) =
        let f = F.normalize f
        let map _ = 1
        let reduce = Seq.sum
        let n1 = F.report dateTree f map reduce
        let n2 =
            T.elements dateTree
            |> (Array.filter <| toPredicate f)
            |> Array.map map
            |> reduce
        n1 = n2


