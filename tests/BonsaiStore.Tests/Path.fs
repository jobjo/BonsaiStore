namespace FSharp.BonsaiStore.Tests

module Path =
    open FsCheck.Xunit
    open FSharp.BonsaiStore.Internal.Tree
    open FSharp.BonsaiStore.Internal.Path
    open Common
    module T = FSharp.BonsaiStore.Internal.Tree

    let private genDates () =
        let r = new System.Random()
        [
            for y in [1900 .. 2100] do
                for m in [1 .. 12] do
                    for d in [1..28] do
                        yield y,m,d
        ]
        |> List.sortBy (fun _ -> r.Next())
        |> Seq.truncate 100
        |> List.ofSeq    

    let find path tree = 
        report [||] (fun x -> [|x|]) (Seq.toArray >> Array.concat) path tree

    let private genDatesTree () =
        let dates = genDates ()
        buildDateTree dates

    let private findSet path tree =
        find path tree
        |> Set.ofArray

    let private (==) xs ys =
        if xs <> ys then
            printfn "Not equals:"
            printfn "%A" xs
            printfn "%A" ys
            false
        else
            true

    let private compare tree path p =
        let xs = find path tree |> Array.collect id |> Set.ofArray
        let ys = Set.ofSeq <| Array.filter p (Array.collect id (T.elements tree))
        if xs = ys then
            true
        else
            printfn "==========================="
            printfn "Obtained by path filtering:"
            printfn "%A" xs
            printfn "Obtained by manual filtering:"
            printfn "%A" ys
            false


    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Stop is identity for <|>`` p =
        let tree = genDatesTree ()
        let p1 = p <|> Stop
        let p2 = Stop <|> p
        let es = findSet p tree
        let es1 = findSet p1 tree
        let es2 = findSet p2 tree
        es == es1 && es1 == es2

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Go is identity for <&>`` p =
        let tree = genDatesTree ()
        let p1 = p <&> Go
        let p2 = Go <&> p
        let es = findSet p tree
        let es1 = findSet p1 tree
        let es2 = findSet p2 tree
        es == es1 && es1 == es2

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Join paths with or`` p1 p2 =
        let tree = genDatesTree ()
        let path = p1 <|> p2
        let es1 = findSet path tree
        let es2 = Set.union (findSet p1 tree)  (findSet p2 tree)
        es1 == es2

    [<Property (Arbitrary = [| typeof<Generators.CustomGenerators> |])>]
    let ``Join paths with and`` (p1: Path) (p2: Path) =
        printfn "A"
        let tree = genDatesTree ()
        printfn "B"
        let path = p1 <&> p2
        printfn "C"
        let es1 = findSet path tree
        printfn "D"
        let es2 = Set.intersect (findSet p1 tree)  (findSet p2 tree)
        printfn "E"
        es1 == es2