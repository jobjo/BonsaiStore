namespace FSharp.BonsaiStore.Internal

module Filter =
    
    open FSharp.Data.Generics
    open Path

    module P = Utils.Printer

    /// Comparison operators.
    type BinOp =
        | EQ
        | LET
        | GET
        override this.ToString () =
            match this with
            | EQ    -> "EQ"
            | LET   -> "LET"
            | GET   -> "GET"

    /// Filter.
    type Filter<'P,'K when 'K : comparison> =
        | True
        | False
        | Property of 'P * BinOp * 'K
        | Not of Filter<'P,'K>
        | And of Filter<'P,'K> * Filter<'P,'K>
        | Or of Filter<'P,'K> * Filter<'P, 'K>

    /// Uniplate instance for Filter.
    let UP<'T,'K when 'K : comparison> = 
        Uniplate.mkUniplate <| fun (f: Filter<'T,'K>) ->
            match f with
            | True              -> [] , fun _ -> True
            | False             -> [], fun _ -> False
            | Property (l,op,c) -> [], fun _ -> Property (l,op,c)
            | Not f             -> [f], List.head >> Not
            | And (f1,f2)       -> [f1; f2], fun fs -> And (fs.[0], fs.[1])
            | Or (f1,f2)        -> [f1; f2], fun fs -> Or (fs.[0], fs.[1])

    /// Pretty print filter.
    let showFilter<'T,'K when 'K : comparison> : Filter<'T,'K> -> string =
        let (!<) = P.(!<)
        let block name tp =
            !<[
                P.print (sprintf "%s(" name)
                P.indent tp
                P.print ")"
            ]
        let rec go = function
            | True              -> 
                P.print "True"
            | False             -> 
                P.print "False"
            | Property(ft,op,c) -> 
                let ops = string <| box op 
                let fts = string <| box ft 
                let cs = string <| box c
                P.print <| sprintf "Property(%s,%s,%s)" fts ops cs
            | Not f             -> 
                block "Not" (go f)
            | And (f1,f2)       -> 
                block "And" !<[go f1; P.print "," ; go f2]
            | Or (f1,f2)        -> 
                block "Or" !<[go f1; P.print ","; go f2]
        go >> P.run

    /// Creates a new property.
    let property op ft k = Property (ft,op, k)

    /// Equality property.
    let equal x = property EQ x

    /// Less-or-equal-than property.
    let lessEqThan x = property LET x
    
    /// Greater-or-equal-than property.
    let greaterEqThan x = property GET x

    /// Anding two filters.
    let (<&>) f1 f2 = And (f1, f2)

    /// Compose two filters with or.
    let (<|>) f1 f2 = Or (f1,f2)

    /// Negating a filter.
    let (!) = Not

    /// Anding a list of filters.
    let all fs = Seq.fold (<&>) True fs

    /// Compose a list of filters with or.
    let any fs = Seq.fold (<|>) False fs

    /// Normalizes filter by applying rewrite rules.
    let normalize<'T, 'K when 'K : comparison and 'T : equality> : Filter<'T,'K> -> Filter<'T,'K> = 
        let reorder op f1 f2 f3 =
            match f1, f2, f3 with
            | Property (p1, _, _), _,  Property (p3, _, _)  when p1 = p3    ->
                match f2 with
                | Property (p2, _, _) when p2 = p1  ->
                    None
                | _                                 ->
                    Some (op (op f1 f3)  f2)
            | _, Property (p2, _, _), Property (p3, _, _)  when p2 = p3     ->
                Some (op f1 (op f2 f3))
            | _                                                             ->
                None
        UP.Rewrite <| function
            | Not True ->
                Some False
            | Not False ->
                Some True
            | Not (Not f) -> 
                Some f
            | Or(False,f)
            | Or(f,False) ->
                Some f
            | Or(True,f)
            | Or(f,True) ->
                Some True
            | And (f1, f2) when f1 = f2 ->
                Some f1
            | Or (f1, f2) when f1 = f2 ->
                Some f1
            | And(False,_)
            | And(_,False) ->
                Some False
            | And(True,f)
            | And(f,True) ->
                Some f
            | And (Property (p1,EQ,k1), Property (p2,EQ,k2)) ->
                // TODO: Add merging of binary operator rules.
                if p1 = p2 then
                    if k1 = k2 then Some True else Some False
                else
                    None
            | And (And(f1,f2), f3)  ->
                reorder (<&>) f1 f2 f3
            | Or (Or(f1,f2), f3)  ->
                reorder (<|>) f1 f2 f3
            | Not (And(f1,f2)) ->
                Some (!f1 <|> !f2)
            | Not (Or(f1,f2))   ->
                Some (!f1 <&> !f2)
            | _                 ->
                None

    /// Converts a filter to a predicate function.
    let rec toPredicate<'P,'K,'T when 'K : comparison> 
                        (f: 'P -> 'T -> 'K) 
                        (filter: Filter<'P,'K>) =
        match filter with
        | True          -> 
            fun _ -> true
        | False         -> 
            fun _ -> false
        | Not ft         -> 
            toPredicate f ft >> not
        | And(f1,f2)    -> 
            fun x -> toPredicate f f1 x && toPredicate f f2 x
        | Or (f1,f2)    -> 
            fun x -> toPredicate f f1 x || toPredicate f f2 x
        | Property (ft,op, c) ->
            let toKey = f ft
            match op with
            | EQ    -> fun x -> toKey x = c
            | GET   -> fun x -> toKey x >= c
            | LET   -> fun x -> toKey x <= c

    /// Extracts the property types.
    let propertyTypes f =
        UP.Universe f
        |> List.choose (function
            | Property (p,_,_)      -> Some p
            | _                     -> None
        )
        |> Set.ofList
        |> Set.toList

    /// Transforms a filter to a path.
    let rec toPath = function
        | True                      ->
            Go
        | False                     ->  
            Stop
        | Property (ft,op, c)       ->
            match op with
            | EQ    ->
                Step {FilterType = ft; BranchSelector = Include [c, Go]}
            | LET   ->
                Step {FilterType = ft; BranchSelector = To (c, Go)}
            | GET   ->
                Step {FilterType = ft; BranchSelector = From (c, Go)}
        | Not f     ->
            let rec negatePath path =
                match path with
                | Path.Go       ->
                    Stop
                | Path.Stop     -> 
                    Go
                | Path.Step s   ->
                    let step bs = Step {s with BranchSelector = bs}
                    match s.BranchSelector with
                    | Include ns        ->
                        let nsMap = Map.ofSeq ns
                        step <| Custom (fun k ->
                            match Map.tryFind k nsMap with
                            | Some p    -> negatePath p
                            | None      -> Go
                        )
                    | Custom f          ->
                        step <| Custom (f >> negatePath )
                    | From(kl,p)        ->
                        step <| Custom (fun k -> if k < kl then Go else negatePath p)
                    | To(kh,p)           ->
                        step <| Custom (fun k -> if k > kh then Go else negatePath p)
                    | Range(kl,kh,p)    -> 
                        step <| Custom (fun k -> if k < kl || k > kh then Go else negatePath p)
            negatePath <| toPath f
        | And (f1, f2)              ->
            joinAnd [toPath f1; toPath f2]
        | Or (f1,f2)                ->
            joinOr [ toPath f1 ; toPath f2]

    /// Given a identity, a mapper and reducer function traverses the
    let report  (mzero: 'R) 
                (map: 'T -> 'R) 
                (reduce: 'R [] -> 'R) 
                (f: Filter<int,int>) 
                (tree: Tree.Tree<int,int,'T>) 
                : 'R =
        report mzero map reduce (toPath f) tree

