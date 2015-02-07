namespace FSharp.BonsaiStore
module internal ExpressionUtils =

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open FSharp.Data.Generics

    type private E = Microsoft.FSharp.Quotations.Expr

    /// Uniplate instance for Expr.
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
                    match eo, es with
                    | Some _, e:: es    -> Expr.PropertyGet (e, pi, es)
                    | _                 -> Expr.PropertyGet( pi,es)
                (Option.toList eo @ es), build
            | IfThenElse(e1,e2,e3)  ->
                [e1;e2;e3], fun es -> Expr.IfThenElse(es.[0], es.[1], es.[2])
            | e                     ->
                [], fun _ -> e

    /// Normalizes an expression by applying some selected rewrite rules.
    let private simplify =
        UP.Rewrite <| function
            | Let (v,e,body)    -> 
                Some (body.Substitute (fun v' -> if v = v' then Some e else None))
            | _                 ->
                None

    // Given an expression replaces the name of all variables to "this".
    let private normalizeVariableName =
        let fParam = "thisParam"
        UP.Rewrite (function
            | Lambda(p,body) when p.Name <> fParam  ->
                Some <| E.Lambda(Var(fParam, p.Type), body)
            | Var v when v.Name <> fParam           ->
                Some (E.Var <| Var(fParam, v.Type))
            | _                                     ->
                None
        )
    
    let normalize : Expr -> Expr = simplify >> normalizeVariableName

    /// Collects values from matching sub expressions.
    let collect f = UP.Universe >> List.choose f