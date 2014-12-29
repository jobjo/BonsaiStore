namespace FSharp.BonsaiStore.Tests

module Tree =
    open Xunit
    open FsCheck.Xunit
    open System.Net
    module T = FSharp.BonsaiStore.Internal.Tree
    module C = Common
    
    let genDates () =
        let r = new System.Random()
        [
            for y in [1900 .. 2000] do
                for m in [1 .. 12] do
                    for d in [1..28] do
                        yield y,m,d
        ]
        |> List.sortBy (fun _ -> r.Next())
        |> Seq.truncate 100
        |> List.ofSeq

//    [<Property>]
//    let ``Retrieve all elements from tree``() =
//        let dates = genDates ()
//        let tree = C.buildDateTree dates
//        let foo = T.elements tree
//        let allDates = 
//            T.elements tree
//            |> Array.concat
//            |> List.ofArray
//        List.sort allDates = List.sort dates
//
//    [<Property>]
//    let ``Insert elements twice`` () =
//        let dates = genDates ()
//        let dates = dates @ dates
//        let tree = C.buildDateTree dates
//        let allDates = 
//            T.elements tree
//            |> Array.concat
//            |> List.ofArray
//        List.sort allDates = List.sort dates
//
//    [<Property>]
//    let ``Map id is identity`` () =
//        let dates = genDates ()
//        let tree = C.buildDateTree dates
//        let tree2 = T.map id tree
//        T.elements tree = T.elements tree2
//
//    [<Property>]
//    let ``Functor composition`` (f: list<int> -> int) g =
//        let dates = genDates ()
//        let tree = C.buildDateTree dates
//        let tree2 = T.map f <| T.map g tree
//        let tree3 = T.map (g >> f) tree
//        T.elements tree2 = T.elements tree3
//
//    [<Property>]
//    let ``Map count sums up to length of list`` () =
//        let dates = genDates ()
//        let tree = C.buildDateTree dates
//        let countTree = T.map Array.length tree
//        List.sum (T.elements countTree) = List.length dates