namespace FSharp.BonsaiStore
module Quatations =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open System.Reflection
    open FSharp.BonsaiStore.Internal.Filter

    module F = FSharp.BonsaiStore.Internal.Filter
    module D = Microsoft.FSharp.Quotations.DerivedPatterns
    module T = FSharp.BonsaiStore.Internal.Tree
    module P = FSharp.BonsaiStore.Internal.Path
    module EU = ExpressionUtils

    let private UP = EU.UP

    type private E = Microsoft.FSharp.Quotations.Expr
    type private Index = {Level : int; ToIndex : obj -> int}
    type private IndexMapper = { MatchExpression : Expr -> Option<Index> }

    /// Empty index mapper
    let private emptyIndexMapper = {MatchExpression = fun _ -> None}
    
    /// Merge a sequence of index matchers.
    let private mergeIndexMappers = 
        let merge ix1 ix2 =
            { MatchExpression = fun e ->
                 match ix1.MatchExpression e with
                 | Some x   -> Some x
                 | None     -> ix2.MatchExpression e
            }
        Seq.fold merge emptyIndexMapper


    /// Given a lambda expression, rewrites it into a key-mapping function.
    /// Ex: genMapKey <@ fun item -> int (item.Product.Price / 100) @> => fun x -> int (x / 100)
    /// Ex: genMapKey <@ fun item -> item.Product.Name.GetHashCode() @> => fun x -> x.GetHashCode()
    let private generateIndexMapper level exp =


        printfn "======================="
        printfn "Exp:"
        printfn "%A" exp

        let exp = EU.normalize exp

        printfn "======================="
        printfn "Normalized Exp:"
        printfn "%A" exp

        // Extracts the longest GET property sub expression involving the formal
        // parameter.
        let property =
            // Size of the expression.
            let rec heightOfGet name = function
                | PropertyGet (oe,pi,es) ->
                    match oe with
                    | Some e    -> Option.map ((+) 1) (heightOfGet name e)
                    | None      -> None
                | Var v                 ->
                    if v.Name = name then Some 0 else None
                | _                     ->
                    None

            // Given a name and a body extract the maximum sub-expression
            // involving the parameter.
            let getProp name body =
                UP.Universe body
                |> List.choose (fun exp ->
                    Option.map (fun h -> (exp, h)) (heightOfGet name exp)
                )
                |> List.sortBy snd
                |> List.rev
                |> List.pick (fst >> Some)
            match exp with
            | Lambda(param, body)   ->
                EU.normalize <| getProp param.Name body
            | _                     ->
                failwith "Failed to get expression from non-lambda"

        // Expression representing the toIndex function.
        let toIndexExp =
            match exp with
            | Lambda(_, body)   ->
                // Rewrites the body of the function
                let fParam = Var("param", property.Type)
                let body2 =
                    body
                    |> UP.Rewrite (function
                        | e when e = property   -> Some (E.Var fParam)
                        | _                     -> None
                    )
                E.Lambda(fParam, body2)
            | _                 -> 
                failwith "Exp1 is not a lambda"

        printfn "Property:"
        printfn "%A" property
        printfn "Index Expression:"
        printfn "%A" toIndexExp
        printfn "==================="

        // Compiled index mapper function.
        let toIndex (x: obj) =
            let tp = x.GetType()
            printfn "Type: [%A]" tp
            let arg =
                if tp = typeof<float> then
                    <@@ (x :?> float) @@>
                elif tp = typeof<int> then
                    <@@ (x :?> int) @@>
                elif tp = typeof<string> then
                    <@@ (x :?> string) @@>
                elif tp = typeof<bool> then
                    <@@ (x :?> bool) @@>
                elif tp = typeof<char> then
                    <@@ (x :?> char) @@>
                elif tp = typeof<System.DateTime> then
                    <@@ x :?> System.DateTime @@>
                else
                    failwith "Type mismatch"
            E.Application(toIndexExp, arg).CompileUntyped()() :?> int

        // Map expression to index expression.
        let matchExp exp =
            let nExp = EU.normalize exp
            printfn "nExp: [%A]" nExp
            printfn "property: [%A]" property
            printfn "Equals: %A" (string nExp = string property)

            if string nExp = string property then
                Some {Level = level; ToIndex =  toIndex}
            else
                None
        { MatchExpression = matchExp }

    /// Create a key filter.
    let private toFilter<'T> (ixMapper : IndexMapper) (exp: Expr<'T -> bool>) =
        let toFilter =
            // Translate binary operator.
            let rec binOp op (exps: list<Expr>) =
                let e1 = exps.[0]
                let e2 = exps.[1]
                match ixMapper.MatchExpression e1, ixMapper.MatchExpression e2 with
                | None, None                            ->
                    None
                | None, _                               ->
                    let op =
                        match op with
                        | BinOp.EQ  -> BinOp.EQ
                        | BinOp.GET -> BinOp.LET
                        | BinOp.LET -> BinOp.GET
                    binOp op [e2; e1]
                | Some index, _                         ->
                    printfn "Found matching index"
                    printfn "e1: [%A]" e1
                    printfn "e2: [%A]" e2
                    let obj = e2.CompileUntyped()()
                    printfn "Object: %A" obj
                    let ix = index.ToIndex obj
                    printfn "Index: [%A]" ix
                    Some <| Property(index.Level,op,ix)


            // Recursively translate expression.
            let rec toFilter exp = 
                match exp with
                | IfThenElse (e1,e2,e3)                 ->
                    match toFilter e1, toFilter e2, toFilter e3 with
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
                    toFilter exps.[0] |> Option.map Filter.Not
                | Value (obj,tp)                        ->
                    if tp = typeof<bool> then
                        let b = obj :?> bool
                        if b then Some True else Some False
                    else
                        failwith "Failed to translate non-boolean value %A" obj
                | _                                     ->
                    None
            fun exp -> 
                match toFilter exp with
                | Some f    -> f
                | None      -> Filter.True

        // Require a lambda expression.
        match exp with
        | Lambda (param, body)  ->
            toFilter body
        | _                     ->
            failwith "Not able to translate non-lambda"

    /// Get indexes from type
    let extractIndexes<'T>() = 
        let tp = typeof<'T>
        let flags = 
            BindingFlags.Public |||
            BindingFlags.NonPublic |||
            BindingFlags.Instance
        [
            for mi in tp.GetMethods(flags) do
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
                            let exp = (Expr.Cast(Expr.Lambda(this,body)) : Expr<'T -> int>)
                            yield level, exp
                        else
                            failwith "Index method must have unit argument."
                    | _                                 ->
                        ()
                | None      -> ()
        ]

    /// Given a list of properties, builds a filter generator.
    let buildFilterGenerator<'T> (indexes: seq<Expr<'T -> int>>)  =

        // Create index mapper from the given indexes.
        let indexMapper = 
            indexes
            |> Seq.mapi (fun ix -> generateIndexMapper ix)
            |> mergeIndexMappers

        // Given an expression 
        fun (exp: Expr<'T -> bool>) ->
            Expr.Cast<'T -> bool>(EU.normalize exp)
            |> toFilter indexMapper
            |> F.normalize

    /// Compile quotation filter.
    let compileQuatationFilter<'T> (exp: Expr<'T -> bool>) = exp.Compile() ()