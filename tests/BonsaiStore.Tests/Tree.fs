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

    let (==) xs ys =  Set.ofSeq xs = Set.ofSeq ys

    [<Property>]
    let ``Retrieve all elements from tree``() =
        let dates = Set.ofSeq <| genDates ()
        let tree = C.buildDateTree dates
        T.elements tree == dates

    [<Property>]
    let ``Insert elements twice`` () =
        let dates = genDates ()
        let dates = dates @ dates
        let tree = C.buildDateTree dates
        T.elements tree == dates

    [<Property>]
    let ``Map id is identity`` () =
        let dates = genDates ()
        let tree = C.buildDateTree dates
        let tree2 = T.map id tree
        T.elements tree = T.elements tree2

    [<Property>]
    let ``Insert many same as build tree`` () =
        let items = genDates ()
        let empty = T.empty<int * int * int>
        let tree1 = C.buildDateTree items
        let tree2 = T.insert C.treeConf C.levels empty items
        let n1 = T.numNodes tree1
        let n2 = T.numNodes tree2
        let h1 = T.height tree1
        let h2 = T.height tree2
        n1 = n2 &&
        h1 = h2 &&
        T.elements tree1 == T.elements tree2

    [<Property>]
    let ``Insert one by one same as build tree`` () =
        let items = genDates ()
        let empty = T.empty<int * int * int>
        let ins tree x = T.insert C.treeConf C.levels tree [x]
        let tree1 = C.buildDateTree items
        let tree2 = Seq.fold ins empty items
        let n1 = T.numNodes tree1
        let n2 = T.numNodes tree2
        let h1 = T.height tree1
        let h2 = T.height tree2
        n1 = n2 &&
        h1 = h2 &&
        T.elements tree1 == T.elements tree2


    [<Property>]
    let ``Filter tree with always true keeps all elements`` () =
        let items = genDates ()
        let tree1 = C.buildDateTree items
        T.elements (T.filter (fun _ -> true) tree1) == items

    [<Property>]
    let ``Filter tree same as filter array`` () =
        let items = genDates ()
        let tree1 = C.buildDateTree items
        let pred (y,_: int,_: int) = y % 2 = 0
        T.elements (T.filter pred tree1) == List.filter pred items

    [<Property>]
    let ``Filter out all elements result in empty tree`` () =
        let items = genDates ()
        let tree1 = C.buildDateTree items
        let tree2 = T.filter (fun _ -> false) tree1
        T.isEmptyLeaf tree2 && T.elements tree2 == [||]

    [<Property>]
    let ``Functor composition`` (f: list<int> -> int) g =
        let dates = genDates ()
        let tree = C.buildDateTree dates
        let tree2 = T.map f <| T.map g tree
        let tree3 = T.map (g >> f) tree
        T.elements tree2 = T.elements tree3