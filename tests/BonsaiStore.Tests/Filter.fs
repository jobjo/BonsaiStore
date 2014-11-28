namespace FSharp.BonsaiStore.Tests

module Filters =
    open FsCheck.Xunit
    open Common

    open FSharp.BonsaiStore.Internal.Filter
    module F = FSharp.BonsaiStore.Internal.Filter


    let private filterByPredicateSet f tree =
        filterByPredicate f tree |> Set.ofArray

    let private findSet f tree =
        F.find f tree |> Array.concat |> Set.ofSeq

    let compareSets f s1 s2 =
        if s1 <> s2 then
            printfn "========================================"
            printfn "[%A]" f
            printfn "Normalized[%A]" (F.normalize f)
            printfn "Path: %A" (F.toPath f)
            printfn "S1: %A" s1
            printfn "S2: %A" s2
            printfn "S2 - S1: %A" (s2 - s1)
            printfn "S1 - S2 %A" (s1 - s2)
            false
        else
            true

//
//    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
//    let ``Normalize twice doesn't change`` (f: F.Filter<DateProperty, int>) =
//        let f1 = F.normalize f
//        f1 = F.normalize f1
//
//    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
//    let ``Normalize filter preserves semantics`` (f: F.Filter<DateProperty, int>) =
//        let s1 = filterByPredicateSet f dateTree
//        let s2 = filterByPredicateSet (F.normalize f) dateTree
//        compareSets f s1 s2
//
//    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
//    let ``Filter by predicate same as filter`` (f: F.Filter<DateProperty, int>) =
//        let s1 = filterByPredicateSet f dateTree
//        let s2 = findSet f dateTree
//        compareSets f s1 s2
//
//    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
//    let ``Intersection of filter same as composing with and`` (f1: F.Filter<DateProperty, int>) (f2: F.Filter<DateProperty, int>) =
//        let f1, f2 = F.normalize f1, F.normalize f2
//        let f = f1 <&> f2
//        let s1 = Set.intersect (filterByPredicateSet f1 dateTree) (filterByPredicateSet f2 dateTree)
//        let s2 = findSet f dateTree
//        compareSets  f s1 s2
//
//    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
//    let ``Union of filter same as composing with or`` (f1: F.Filter<DateProperty, int>) (f2: F.Filter<DateProperty, int>) =
//        let f1, f2 = F.normalize f1, F.normalize f2
//        let s1 = Set.union (findSet f1 dateTree) (findSet f2 dateTree)
//        let s2 = findSet (F.Or(f1,f2)) dateTree
//        s1 = s2

