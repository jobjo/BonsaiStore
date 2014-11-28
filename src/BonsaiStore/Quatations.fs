namespace FSharp.BonsaiStore
module Quatations =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open FSharp.Data.Generics

    module D = Microsoft.FSharp.Quotations.DerivedPatterns

    open FSharp.BonsaiStore.Internal.Filter
    module T = FSharp.BonsaiStore.Internal.Tree
    module P = FSharp.BonsaiStore.Internal.Path

    /// Uniplate instance for Expr
    let UP =
        Uniplate.mkUniplate <| function
            | Application(e1,e2)    -> 
                [e1; e2], fun es -> Expr.Application(es.[0], es.[1])
            | Call(eo,mi,es)        -> 
                match eo with
                | Some e    -> e :: es, fun es -> Expr.Call (es.[0], mi, List.tail es)
                | None      -> es, fun es -> Expr.Call (mi, es)
            | Lambda(param, body)   ->
                [body], fun es -> Expr.Lambda(param, es.[0])
            | Let(v, e, body)     ->
                [e; body], fun es -> Expr.Let(v, es.[0], es.[1])
            | PropertyGet(eo,pi,es) ->
                let build (es: list<Expr>) =
                    match eo with
                    | Some e    -> Expr.PropertyGet (e, pi, es)
                    | None      -> Expr.PropertyGet( pi,es)
                es, build
            | IfThenElse(e1,e2,e3)  ->
                [e1;e2;e3], fun es -> Expr.IfThenElse(es.[0], es.[1], es.[2])
            | e                     ->
                [], fun _ -> e
                
    /// Normalizes an expression by applying some selected rewrite rules.
    let simplify<'T> (exp: Expr<'T>)=
        let rewrite =
            UP.Rewrite <| function
                | Let (v,e,body)    -> 
                    Some (body.Substitute (fun v' -> if v = v' then Some e else None))
                | _                 ->
                    None
        let exp = rewrite (exp :> Expr)
        Expr.Cast<'T>(exp)

    /// Collects values from matching sub expressions.
    let collect f = UP.Universe >> List.choose f

    /// Returns property type of a property get function.
    let propertyType = function
        | PropertyGet (_,pi,_)  -> Some pi.PropertyType
        | _                     -> None

    /// Given an expression returns a tuple of the identifier name and property 
    /// lookup if exists.
    /// Ex: getIdentifier <@ foo.Bar.Baz @> => Some ("foo", "Bar.Baz")
    ///     getIdentifier <@ fun x -> 1 @> => None
    /// TODO: Also match method calls.
    let private getIdentifier exp =
        let rec go = function
            | Var v                 ->
                [[v.Name]]
            | PropertyGet (eo,pi,_) ->
                let pis =
                    match eo with
                    | Some p    -> go p
                    | _         -> []
                match pis with
                | [ps]  -> [pi.Name :: ps]
                | _     -> [[pi.Name]]
            | _                     ->
                []
        let pType = propertyType exp 
        match go exp, pType with
        | [xs], Some t  -> 
            match List.rev xs with
            | x :: xs   -> Some (x, System.String.Join (".",xs), t)
            | _         -> None
        | _     -> 
            None

    /// Given a lambda expression, returns a property and type pair, if exists.
    let private findProperty exp =
        match exp with
        | Lambda(param, body)   ->
            collect getIdentifier body
            |> List.tryPick (fun (id, prop, tp) ->
                if id = param.Name then Some (prop, tp) else None
            )
        | _                     ->
            None

    /// Given an expression, find the type of a property of an identifier 
    /// with the given name.
    /// Ex: findType "item" <@ item.Price.Length @> ==> int32
    let rec private findType pName  (exp: Expr) =
        UP.Universe exp
        |> List.tryPick (fun exp ->
            match exp with
            | PropertyGet e ->
                let () = ()
                match getIdentifier exp with
                | Some (n, prop, tp ) when n = pName    -> Some tp
                |_                                      -> None
            | _             ->
                None        
        )

    /// Given a lambda expression, rewrites it into a key-mapping function.
    /// Ex: genMapKey <@ fun item -> int (item.Product.Price / 100) @> => fun x -> int (x / 100)
    /// Ex: genMapKey <@ fun item -> item.Product.Name.GetHashCode() @> => fun x -> x.GetHashCode()
    let private genMapKey<'K> exp =
        // Replace references to var with an expression
        // Same as substitute?
        let rec replace pName (e:Expr) (exp: Expr) = 
            exp.Substitute (fun v -> if v.Name = pName then Some e else None)

        let diggOutType exp =
            match exp with
            | Lambda(param, body)   -> 
                match findType param.Name body with
                | Some x    -> x
                | None      -> failwith "Failed to digg out type"
            |_                      -> 
                failwith "Failed to rewrite lambda"

        let rewrite exp exp2=
            match exp with
            | Lambda(param, body)                   ->
                match findType param.Name body with
                | Some tp ->
                    let temp exp =
                        match getIdentifier exp with
                        | Some (n,_,_) when n = param.Name  ->
                            Some exp2
                        | _                     ->
                            None
                    let body2 = 
                        body
                        |> UP.Rewrite temp
                    body2
                | _         ->
                    failwith "Failed to rewrite lambda"
            |_                                     ->
                failwith "Cannot rewrite non-lambda"
        let tp = diggOutType exp
        fun (x:obj) ->
            if x.GetType() = tp then
                let exp =
                    if tp = typeof<float> then
                        // Float
                        rewrite exp <@ (x :?> float) @>
                    elif tp = typeof<int> then
                        // Int
                        rewrite exp <@ (x :?> int) @>
                    elif tp = typeof<string> then
                        // String
                        rewrite exp <@ (x :?> string) @>
                    elif tp = typeof<bool> then
                        // Bool
                        rewrite exp <@ (x :?> bool) @>
                    elif tp = typeof<char> then
                        // Char
                        rewrite exp <@ (x :?> char) @>
                    else
                        failwith "Type mismatch"
                exp.CompileUntyped()() :?> 'K
            else
                failwith "Type mismatch"

    /// Given a list of level-properties, generates a lookup function from
    /// identifier to pair of level and mapping function.
    let genPropertyLookup<'T, 'K when 'K : comparison> (props: seq<Expr<'T -> 'K>>) =
        let taggedProps = Seq.mapi (fun i p -> (i,simplify p)) props
        let map =
            [
                for (level, exp) in taggedProps do
                    let mapKey = 
                        let e2 = simplify<'T -> 'K> exp
                        genMapKey<'K> e2
                    match findProperty exp with
                    | Some (ident, tp)  ->
                        yield ident, (level, mapKey)
                     |_                 ->
                        ()   
            ]
            |> Map.ofList
        fun k -> Map.tryFind k map

    /// Returns the value of a literal.
    let getLiteral = function
        | Value(value, typ) -> Some value
        | _                 -> None 
    
    /// Create a key filter.
    let private toKeyFilter<'T, 'K when 'K : comparison>
                    (propertyLookup: string -> option<int * (obj -> 'K)>) 
                    (exp: Expr<'T -> bool>) =
        
        let toFilter pName =
            // Translate binary operator.
            let rec binOp op (exps: list<Expr>) =
                let e1 = exps.[0]
                let e2 = exps.[1]
            
                match getIdentifier e1, getIdentifier e2 with
                | None, None                                ->
                    None
                | None, _                                      ->  
                    let op =
                        match op with
                        | BinOp.EQ  -> BinOp.EQ
                        | BinOp.GET -> BinOp.LET
                        | BinOp.LET -> BinOp.GET
                    binOp op [e2; e1]
                | Some (name, prop, tp), _ when name = pName   ->
                    match getLiteral e2 with
                    | Some v    ->
                        match propertyLookup prop with
                        | Some (level, mapToKey)    ->
                            let k = 
                                try
                                    mapToKey (v)
                                with
                                | m     ->
                                    failwith <| sprintf "Failed to map to key: %s" m.Message
                            Some <| Property(level, op, k)
                        | None                      ->
                            None
                    | None      ->
                        None
                | Some (name, prop, tp), _                     ->
                    None

            let rec go exp = 
                match exp with
                | IfThenElse (e1,e2,e3)                 ->
                    match go e1, go e2, go e3 with
                    | Some f1, Some f2, Some f3 ->
                        Some ((f1 <&> f2) <|> f3)
                    | Some f1, Some f2, None        ->
                        Some (f2 <|> !f1)
                    | Some f1, None, Some f3        ->
                        Some (f1 <|> f3)
                    | _                             ->
                        Some True
                | SpecificCall <@ (=) @> (_,ts, exps)   ->
                    binOp BinOp.EQ exps
                | SpecificCall <@ (>) @> (_,ts, exps)   ->
                    binOp BinOp.GET exps
                | SpecificCall <@ (>=) @> (_,ts, exps)   ->
                    binOp BinOp.GET exps
                | SpecificCall <@ (<) @> (_,ts, exps)   ->
                    binOp BinOp.LET exps
                | SpecificCall <@ (<=) @> (_,ts, exps)   ->
                    binOp BinOp.LET exps
                | SpecificCall <@ (<>) @> (_,ts, exps)  ->
                    binOp BinOp.EQ exps |> Option.map Filter.Not
                | SpecificCall <@ not @> (_,ts, exps)   -> 
                    go exps.[0] |> Option.map Filter.Not
                | Value (obj,tp)                        ->
                    if tp = typeof<bool> then
                        let b = obj :?> bool
                        if b then Some True else Some False
                    else
                        failwith "Failed to translate non-boolean value %A" obj
                | exp                                   ->
                    None
            fun exp -> 
                match go exp with
                | Some f    -> f
                | None      -> Filter.True
        
        match exp with
        | Lambda (param, body)  ->
            toFilter param.Name body
        | _                     ->
            failwith "Not able to translate non-lambda"
        
    /// Given a list of properties, builds a filter generator.
    let buildFilterGenerator<'K, 'T when 'K : comparison> (indexes: seq<Expr<'T -> 'K>>)  = 
        let lookup = genPropertyLookup indexes
        fun (exp: Expr<'T -> bool>) ->
            simplify exp
            |> toKeyFilter lookup
            |> normalize

    /// Get indexes from type
    let extractIndexes<'T,'K>() = 
        let tp = typeof<'T>
        [        
            for mi in tp.GetMethods() do
                match Expr.TryGetReflectedDefinition mi with
                | Some exp    ->
                    // Find level attribute.
                    let level =
                        let oLevel =
                            mi.CustomAttributes
                            |> Seq.tryPick (fun a -> 
                                if a.AttributeType = typeof<Level> then
                                    a.ConstructorArguments
                                    |> Seq.tryPick (fun ta ->
                                        Some (ta.Value :?> int)
                                    )
                                else
                                    None
                            )
                        match oLevel with
                        | Some l    -> l
                        | None      -> 0

                    match exp with
                    | Lambda (this, Lambda(v, body))    ->
                        if v.Type = typeof<unit> then
                            yield level, (Expr.Cast(Expr.Lambda(this,body)) : Expr<'T -> 'K>)
                        else
                            failwith "Index method must have unit argument."
                    | _                                 ->
                        ()
                | None      -> ()
        ]

    /// Compile quotation filter.
    let compileQuatationFilter<'T> (exp: Expr<'T -> bool>) = exp.Compile() ()
