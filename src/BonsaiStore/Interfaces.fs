namespace FSharp.BonsaiStore

[<AutoOpen>]
module Interfaces =
    open Microsoft.FSharp.Quotations

    /// Attribute for indicating that a member method is an index.
    type Index = ReflectedDefinitionAttribute

    /// Attribute to indicate that an index may be used for ranges.
    type Range() = 
        inherit System.Attribute()


    /// Attribute assigning a level for an index.
    type Level(l: int) =
        inherit System.Attribute()
        member this.Level = l

    /// Build configuration.
    type BuildStoreConfiguration =
        {
            /// If set to true, cache all leaf nodes at each level.
            /// This may speed up reporting but increases memory footprint.
            UseCaching : bool
        }
        static member Default = {UseCaching = false}
    
    /// Interface for reporting
    type IStore<'T> =
        /// Report using filter map and reduce.
        abstract Report : Expr<'T -> bool> -> ('T -> 'R) -> ('R [] -> 'R) -> 'R

        /// Inserts a sequence of elements to a store.
        abstract Insert : (seq<'T>) -> IStore<'T>

        /// Filters the elements of the store.
        abstract Filter : ('T -> bool) -> IStore<'T>

        /// Maps over all elements.
        abstract Map : ('T -> 'T) -> IStore<'T>
