namespace FSharp.BonsaiStore.Internal
module Traverse =
    open Tree
    open Path

    /// Select all elements from a tree matching the given path.
    /// TODO: Optimize. Avoid quadratic list merging.
    let rec find (path: Path<'K,'FT>) (tree: Tree<'K,'FT,'T>) =
        match path with
        | Go        ->
            Tree.elements tree
        | Stop      ->
            []
        | Step step ->
            match tree with
            | Tree.Node n  when n.Level = step.FilterType   ->
                match step.BranchSelector with
                | Include nodes  ->
                    [
                        for k,p in nodes do
                            // Lookup the node
                            match n.Children.Lookup k with
                            | Some childTree    ->
                                yield! find p childTree
                            | None              ->
                                ()
                    ]
                | Custom f          ->
                    n.Children.Elements()
                    |> List.collect (fun (k,t) ->
                        find (f k) t
                    )
                | From (kl,p)        ->
                    n.Children.LookupRange (Some kl) None
                    |> List.collect (find p)
                | To (kh,p)        ->
                    n.Children.LookupRange None (Some kh)
                    |> List.collect (find p)
                | Range (kl,kh,p)   ->
                    n.Children.LookupRange (Some kl) (Some kh)
                    |> List.collect (find p)
            | Tree.Node n           ->
                n.Children.Elements()
                |> List.collect (fun (_,t) -> find path t)
            | Tree.Leaf x ->
                // We reached the bottom of the tree so table is returned.
                [x]

    let mapReduce<'K,'FT,'T, 'R when 'K : comparison and 'FT : comparison> 
                    (empty: 'R) 
                    (map: 'T -> 'R) 
                    (reduce: seq<'R> -> 'R)
                    (path: Path<'K,'FT>) 
                    (tree: Tree<'K,'FT,'T>) : 'R =
        let rec go path tree =
            match path with
            | Go        ->
                Tree.elements tree
                |> List.map map
                |> reduce
            | Stop      ->
                empty
            | Step step ->
                match tree with
                | Tree.Node n  when n.Level = step.FilterType   ->
                    match step.BranchSelector with
                    | Include nodes  ->
                        nodes
                        |> List.choose (fun (k,p) ->
                            match n.Children.Lookup k with
                            | Some childTree    -> Some <| go p childTree
                            | None              -> None
                        )
                        |> reduce
                    | BranchSelector.Custom f   ->
                        n.Children.Elements()
                        |> Seq.map (fun (k,t) ->go (f k) t)
                        |> reduce
                    | From (kl,p)        ->
                        n.Children.LookupRange (Some kl) None
                        |> List.map (go p)
                        |> reduce
                    | To (kh,p)        ->
                        n.Children.LookupRange None (Some kh)
                        |> List.map (go p)
                        |> reduce
                    | Range (kl,kh,p)   ->
                        n.Children.LookupRange (Some kl) (Some kh)
                        |> List.map (go p)
                        |> reduce
                | Tree.Node n           ->
                    // Continue to the next level.
                    n.Children.Elements()
                    |> List.map (snd >>  go path)
                    |> reduce
                | Tree.Leaf x ->
                    // We reached the bottom of the tree so table is returned.
                    map x
        go path tree                                        