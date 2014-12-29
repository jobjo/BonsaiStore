namespace FSharp.BonsaiStore.Tests

module MapReduce =
    open FsCheck.Xunit

    module F = FSharp.BonsaiStore.Internal.Filter
    module T = FSharp.BonsaiStore.Internal.Tree
    open Common

    let find path tree = F.report [||] (fun x -> [|x|]) (Seq.toArray >> Array.concat) path tree

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Map-reduce same as filter, map and reduce`` (f: F.Filter<DateProperty, int>) =
        let f = F.normalize f
        let mzero = 0
        let map = Array.length
        let reduce = Seq.sum
        let n1 = F.report mzero map reduce f dateTree
        let n2 =
            T.elements dateTree
            |> Array.map (Array.filter <| toPredicate f)
            |> Array.map map
            |> reduce
        n1 = n2

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Map-reduce same as find, map and reduce`` (f: F.Filter<DateProperty, int>) =
        let f = F.normalize f
        let mzero = 0
        let map = Array.length
        let reduce = Seq.sum
        let n1 = F.report mzero map reduce f dateTree
        let n2 = find f dateTree |> Array.map map |> reduce
        n1 = n2

