namespace FSharp.BonsaiStore.Internal

module Path =
    open Tree
    module P = Utils.Printer

    /// Represents an instruction for selecting a set of branches.
    type BranchSelector =
        | Include of list<Key * Path>
        | Custom of (Key -> Path)
        | From of Key * Path
        | To of Key * Path
        | Range of Key * Key * Path

    /// A path consists of a filter type denoting the level in tree,
    /// along with a filter.
    and Step = 
        { Level : Level ; BranchSelector : BranchSelector }

    /// A Path is either an go, stop or step instruction.
    and Path =
        | Go
        | Stop
        | Step of Step

    /// Pretty prints a path.
    let showPath<'K,'T when 'K : comparison> =
        let (!<) = P.(!<)
        let rec printFilter (f: BranchSelector) =
            match f with
            | Include ns ->
                !<[
                    for (k,p) in ns do
                        yield P.print (sprintf "Include[%s] [" (string <| box k))
                        yield P.indent <| printPath p
                        yield P.print "]"
                ]
            | Custom _ ->
                P.print (sprintf "Custom")
            | From (k,path)         ->
                !<[
                    P.print (sprintf "From[%s] [" (string <| box k))  
                    P.indent<| printPath path
                    P.print "]"
                ]
            | To (k,path)         ->
                !<[
                    P.print (sprintf "To[%s] [" (string <| box k))  
                    P.indent <| printPath path
                    P.print "]"
                ]
            | Range (kl,kh,path)    ->
                !<[
                    P.print (sprintf "Range[%s,%s] [" (string <| box kl) (string <| box kh))
                    P.indent <| printPath path
                    P.print "]"                    
                ]

        and printPath (p: Path) =
            match p with
            | Go        ->
                P.print "Go"
            | Stop      ->
                P.print "Stop"
            | Step step ->
                let ft =  step.Level
                !<[
                    P.print (sprintf "%s [" (string (box ft)))
                    P.indent <| printFilter step.BranchSelector
                    P.print "]"                
                ]
        printPath >> P.run

    /// Report over a tree.
    let report  (tree: Tree<'T>)
                (path: Path) 
                (map: 'T -> 'R) 
                (reduce: 'R [] -> 'R) 
                : 'R =
        let empty = reduce [||]
        let rec go path tree =
            match path with
            | Go        ->
                Tree.elements tree
                |> Array.Parallel.map map
                |> reduce
            | Stop      ->
                empty
            | Step step ->
                match tree with
                | Tree.Node n  when n.Level = step.Level   ->
                    match step.BranchSelector with
                    | Include nodes  ->
                        nodes
                        |> Array.ofList
                        |> Array.Parallel.choose (fun (k,p) ->
                            match n.Children.Lookup k with
                            | Some childTree    -> Some <| go p childTree
                            | None              -> None
                        )
                        |> reduce
                    | BranchSelector.Custom f   ->
                        n.Children.Elements()
                        |> Array.Parallel.map (fun (k,t) ->go (f k) t)
                        |> reduce
                    | From (kl,p)        ->
                        n.Children.LookupRange (Some kl) None
                        |> Array.Parallel.map (go p)
                        |> reduce
                    | To (kh,p)        ->
                        n.Children.LookupRange None (Some kh)
                        |> Array.Parallel.map (go p)
                        |> reduce
                    | Range (kl,kh,p)   ->
                        n.Children.LookupRange (Some kl) (Some kh)
                        |> Array.Parallel.map (go p)
                        |> reduce
                | Tree.Node n           ->
                    // Continue to the next level.
                    n.Children.Elements()
                    |> Array.Parallel.map (snd >>  go path)
                    |> reduce
                | Tree.Leaf xs ->
                    // We reached the bottom of the tree.
                    xs
                    |> Array.Parallel.map map
                    |> reduce
        go path tree

    /// Compare two paths. Returns None if any of the given
    /// paths contains a custom branch.
    let rec samePaths p1 p2 : Option<bool> =
        match p1, p2 with
        | Go, Go            -> 
            Some true
        | Stop, Stop        -> 
            Some true 
        | Step s1, Step s2  ->
            if s1.Level = s2.Level then
                match s1.BranchSelector, s2.BranchSelector with
                    | Custom _, _
                    | _, Custom _                           -> 
                        None                    
                    | Include is1, Include is2              ->
                        let compare xs ys =
                            [
                                for (k1,p1) in xs do
                                    match List.tryFind (fun (k2,p2) -> k1 = k2) ys with
                                    | Some (_, p2)  -> yield samePaths p1 p2
                                    | _             -> yield Some false
                            ]
                        let rs = compare is1 is2  @ compare is2 is1
                        if List.exists ((=) (Some false)) rs then
                            Some false
                        elif List.forall ((=) (Some true)) rs then
                            Some true
                        else
                            None
                    | From(k1,p1), From(k2,p2)              ->
                        if k1 = k2 then samePaths p1 p2 else Some false
                    | To(k1,p1), To(k2,p2)                  ->
                        if k1 = k2 then samePaths p1 p2 else Some false
                    | Range (l1,h1,p1), Range (l2,h2,p2)    ->
                        if l1 = l2 && h1 = h2 then samePaths p1 p2 else Some false
                    | _             ->
                        Some false
            else
                Some false
        | _             ->
            Some false

    let private range l h p  =
        if l < h then
            Range (l,h,p)
        elif l = h then
            Include [l,p]
        else
            Include []

    /// Join two paths so that the resulting path will include a node if
    /// and only if it's selected by both of the given paths.
    let rec (<&>) p1 p2 =
        match p1, p2 with
        | Go, p             ->
            p
        | p, Go             ->
            p
        | Stop, _
        | _, Stop           ->
            Stop
        | Step s1, Step s2  ->
            let step = function
                | Include []    -> Stop
                | bs            -> Step {s1 with BranchSelector = bs}
            // Check if the levels of the steps are matching.
            if s1.Level = s2.Level then
                match s1.BranchSelector, s2.BranchSelector with
                | Include is1, Include is2              ->
                    let nodes =
                        [
                            for (k1,p1) in is1 do
                                for (k2,p2) in is2 do
                                    if k1 = k2 then yield (k1, p1 <&> p2)
                        ]
                    step <| Include nodes
                | Include is, Custom f                  ->
                    let nodes = 
                        [ for (k,p1) in is do yield (k, p1 <&> f k)]
                    step <| Include nodes
                | Include is, From (kl,p2)              ->
                    let nodes = 
                        [ for (k,p1) in is do 
                            if k >= kl then yield (k, p1 <&> p2)]
                    step <| Include nodes
                | Include is, To (kl,p2)                ->
                    let nodes = 
                        [for (k,p1) in is do 
                            if k <= kl then yield (k, p1 <&> p2)]
                    step <| Include nodes
                | Include is, Range (kl,kh,p2)          ->
                    let nodes = 
                        [for (k,p1) in is do 
                            if k >= kl && k <= kh then yield (k, p1 <&> p2)]
                    step <| Include nodes
                | Custom f1, Custom f2                  ->
                    step (Custom <| fun k -> f1 k <&> f2 k)
                | Custom f, From (kl, p2)               ->
                    step (Custom <| fun k -> if k >= kl then f k <&> p2 else Stop)
                | Custom f, To (kl, p2)                 ->
                    step (Custom <| fun k -> if k <= kl then f k <&> p2 else Stop)
                | Custom f, Range (kl,kh, p2)           ->
                    step (Custom <| fun k -> if k >= kl && k <= kh then f k <&> p2 else Stop)
                | From (k1,p1), From (k2,p2)            ->
                    step <| From (max k1 k2, p1 <&> p2)
                | From (k1,p1), To (k2,p2)              ->
                    step <| range k1 k2 (p1 <&> p2)
                | From (k1, p1), Range (kl,kh,p2)       ->
                    step <| range (max k1 kl) kh (p1 <&> p2)
                | To (k1,p1), To(k2,p2)                 ->
                    step <| To (min k1 k2, p1 <&> p2)
                | To (k1,p1), Range(k2l,k2h,p2)         ->
                    step <| range k2l (min k1 k2h) (p1 <&> p2)
                | Range (k1l,k1h,p1), Range(k2l,k2h,p2) ->
                    let l = max k1l k2l
                    let h = min k1h k2h
                    step <| range l h (p1 <&> p2)
                | b1, b2                                ->
                    p2 <&> p1
            elif s1.Level < s2.Level then
                match s1.BranchSelector with
                | Include is        ->
                    let fs = [for (k,p) in is do yield (k, p <&> p2)]
                    step <| Include [ for (k,p) in is do  yield (k, p <&> p2)]
                | Custom f          ->
                    step <| Custom (fun k -> 
                        let p = f k
                        p <&> p2
                    )
                | From (k,p1)       ->
                    step <| From (k, p1 <&> p2)
                | To (k,p1)         ->
                    step <| To (k, p1 <&> p2)
                | Range (l,h,p1)    ->
                    step <| range l h (p1 <&> p2)
            else
                p2 <&> p1

    /// Joins two paths so that the resulting path will include a node if
    /// and only if it's selected by at least one of the given paths.
    let rec (<|>) p1 p2 =
        match p1, p2 with
        | Go, p             ->
            Go
        | p, Go             ->
            Go
        | Stop, p
        | p, Stop           ->
            p
        | Step s1, Step s2  ->
            let step bs = Step {s1 with BranchSelector = bs}
            let nextIncludePath is k =
                is 
                |> List.choose (fun (k1,p) -> if k = k1 then Some p else None)
                |> List.fold (<|>) Stop
            // Check if the levels of the steps are matching.
            if s1.Level = s2.Level then
                match s1.BranchSelector, s2.BranchSelector with
                | Include is1, Include is2              ->
                    let is = is1 @ is2
                    is
                    |> List.map fst 
                    |> Set.ofList 
                    |> List.ofSeq 
                    |> List.map (fun k -> (k, nextIncludePath is k))
                    |> Include
                    |> step
                | Include is, Custom f                  ->
                    step <| Custom (fun k -> f k <|> (nextIncludePath is k))
                | Include is, From (kl,p2)              ->
                    Custom (fun k -> 
                        let ip = nextIncludePath is k
                        if k < kl then ip else ip <|> p2
                    )
                    |> step
                | Include is, To (kl,p2)                ->
                    Custom (fun k -> 
                        let ip = nextIncludePath is k
                        if k > kl then ip else (ip <|> p2)
                    )
                    |> step
                | Include is, Range (kl,kh,p2)          ->
                    Custom (fun k -> 
                        let ip = nextIncludePath is k
                        if k < kl || k > kh then ip else (ip <|> p2)
                    )
                    |> step
                | Custom f1, Custom f2                  ->
                    step (Custom <| fun k -> f1 k <|> f2 k)
                | Custom f, From (kl, p2)               ->
                    Custom (fun k -> 
                        let p1 = f k in if k < kl then p1 else p1 <|> p2
                    )
                    |> step
                | Custom f, To (kl, p2)                 ->
                    Custom (fun k -> 
                        let p1 = f k in if k > kl then p1 else p1 <|> p2
                    )
                    |> step
                | Custom f, Range (kl,kh, p2)           ->
                    Custom (fun k -> 
                        let p1 = f k in if k < kl || k > kh then p1 else p1 <|> p2
                    )
                    |> step
                | From (l1,p1), From (l2,p2)            ->
                    Custom (fun k ->
                        let p1 = if k >= l1 then p1 else Stop
                        let p2 = if k >= l2 then p2 else Stop
                        p1 <|> p2
                    )
                    |> step
                | From (l,p1), To (h,p2)              ->
                    Custom (fun k ->
                        let p1 = if k >= l then p1 else Stop
                        let p2 = if k <= h then p2 else Stop
                        p1 <|> p2
                    )
                    |> step
                | From (l1, p1), Range (l2,h,p2)       ->
                    Custom (fun k ->
                        let p1 = if k >= l1 then p1 else Stop
                        let p2 = if k >= l2 && k <= h then p2 else Stop
                        p1 <|> p2
                    )
                    |> step
                | To (h1,p1), To(h2,p2)                 ->
                    Custom (fun k ->
                        let p1 = if k <= h1 then p1 else Stop
                        let p2 = if k <= h2 then p2 else Stop
                        p1 <|> p2
                    )
                    |> step
                | To (h1,p1), Range(l,h2,p2)         ->
                    Custom (fun k ->
                        let p1 = if k <= h1 then p1 else Stop
                        let p2 = if k >= l && k <= h2 then p2 else Stop
                        p1 <|> p2
                    )
                    |> step
                | Range (l1,h1,p1), Range(l2,h2,p2) ->
                    Custom (fun k ->
                        let p1 = if k >= l1 && k <= h1 then p1 else Stop
                        let p2 = if k >= l2 && k <= h2 then p2 else Stop
                        p1 <|> p2
                    )
                    |> step
                | b1, b2                            ->
                    p2 <|> p1
            elif s1.Level < s2.Level then
                match s1.BranchSelector with
                | Include is        ->
                    step <| Custom (fun k ->  nextIncludePath is k <|> p2)
                | Custom f          ->
                    step <| Custom (fun k -> f k <|> p2)
                | From (l,p1)       ->
                    Custom (fun k ->
                        let p1 = if k >= l then p1 else Stop
                        p1 <|> p2
                    )
                    |> step
                | To (h,p1)         ->
                    Custom (fun k ->
                        let p1 = if k <= h then p1 else Stop
                        p1 <|> p2
                    )
                    |> step
                | Range (l,h,p1)    ->
                    Custom (fun k ->
                        let p1 = if k >= l && k <= h then p1 else Stop
                        p1 <|> p2
                    )
                    |> step
            else
                p2 <|> p1

    /// Merges a list of paths with or.
    let joinOr ps = Seq.fold (<|>) Stop ps

    /// Merges a list of paths with and.
    let joinAnd ps = Seq.fold (<&>) Go ps

