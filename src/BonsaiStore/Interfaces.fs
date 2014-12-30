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

    /// Interface for reporting
    type IStore<'T> =
        abstract Report<'R> : Expr<'T -> bool> -> ('T -> 'R) -> ('R [] -> 'R) -> 'R