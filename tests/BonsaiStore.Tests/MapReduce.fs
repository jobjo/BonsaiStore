namespace FSharp.BonsaiStore.Tests

module MapReduce =
    open FsCheck.Xunit

    module F = FSharp.BonsaiStore.Internal.Filter
    module T = FSharp.BonsaiStore.Internal.Tree
    open Common

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Map-reduce same as filter, map and reduce`` (f: F.Filter) =
        let f = F.normalize f
        let mzero = 0
        let map _ = 1
        let reduce = Seq.sum
        let n1 = F.report mzero map reduce f dateTree
        let n2 =
            T.elements dateTree
            |> (Array.filter <| toPredicate f)
            |> Array.map map
            |> reduce
        n1 = n2


