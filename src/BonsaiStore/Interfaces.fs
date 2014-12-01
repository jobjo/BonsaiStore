namespace FSharp.BonsaiStore

[<AutoOpen>]
module Interfaces =
    open Microsoft.FSharp.Quotations

    /// Attribute for indicating that a member method is an index.
    type Index = ReflectedDefinitionAttribute

    /// Attribute assigning a level for an index.
    type Level(l: int) =
        inherit System.Attribute()
        member this.Level = l

    /// Identity, map and reduce wrapper.
    type MapReducer<'T,'R> =
        {
            Empty : 'R
            Map : 'T -> 'R
            Reduce : seq<'R> -> 'R
        }

    /// Interface for reporting
    type IBonsaiStore<'T,'K when 'K : comparison> =
        abstract Report<'R> : Expr<'T -> bool> -> MapReducer<'T,'R> -> 'R
        abstract Filter<'T> : Expr<'T -> bool> -> seq<'T>
        abstract Add : 'T -> IBonsaiStore<'T,'K>