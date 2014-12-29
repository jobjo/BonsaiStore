namespace FSharp.BonsaiStore.Internal

module Tree =
    
    open FSharp.Collections.RangeMap
    module P = Utils.Printer
    let private (!<) = P.(!<)

    type Level = int
    type Key = int

    /// A node has a level attribute and a map to child nodes.
    type Node<'T> =
        {
            /// Filter type
            Level : Level

            /// Ability to cache elements
            AllElements : option<'T []>

            /// Child nodes
            Children : IRangeMap<Key, Tree<'T>>
        }

    /// A Tree is either a node or a leaf.
    and Tree<'T> =
        | Node of Node<'T>
        | Leaf of 'T []

    /// Functor instance for tree.
    let rec map f = function
        | Node n -> 
            Node {
                Level = n.Level
                AllElements = Option.map (Array.map f) n.AllElements
                Children =  n.Children.Map (fun t -> map f t) 
            }
        | Leaf x    -> 
            Leaf <| Array.map f x

    /// Pretty prints a tree.
    let showTree<'T> =
        let rec go (tree: Tree<'T>) =
            match tree with
            | Leaf (x: 'T [])    ->
                P.print <| sprintf "Num[%A]" (Array.length x)
            | Node (node: Node<'T>) ->
                !< [
                    yield P.print (sprintf "[%s]" <| node.Level.ToString())
                    for (k,v) in node.Children.Elements() do
                        yield P.print (sprintf "%s" (k.ToString()))
                        yield P.indent <| go v
                ]
        go >> P.run

    /// Configurations for building a tree.
    type IBuildTreeConfiguration =
        abstract CacheElements : unit -> bool
        abstract BuildMap<'K,'V when 'K : comparison> : seq<'K * 'V> -> IRangeMap<'K,'V>

    /// Default tree configuration.
    let defaultBuildTreeConfiguration =
        { new IBuildTreeConfiguration with 
            member this.CacheElements() = false
            member this.BuildMap<'K,'V when 'K : comparison> xs : IRangeMap<'K,'V> = 
                RangeMap.fromSeq xs }

    /// Builds a tree from a list of levels and elements.
    let buildTree (conf: IBuildTreeConfiguration) levels elements  =
        let rec go (items: 'T []) filterTypes =
            match filterTypes with
            | []                        ->
                Tree.Leaf items
            | (level,extract) :: fts    ->
                let children =
                    items
                    |> Seq.groupBy extract
                    |> Seq.map (fun (code, deals) ->
                        code, go (Array.ofSeq deals) fts
                    )
                    |> conf.BuildMap
                Tree.Node {
                    Level = level
                    AllElements = if conf.CacheElements() then Some items else None
                    Children = children
                }
        go (Array.ofSeq elements) levels

    /// Extract all elements from a tree.
    let elements (tree: Tree<'T>) : 'T [] =
        match tree with
        | Leaf (xs: 'T []) -> 
            xs
        | Node n ->
            // Check if elements are accumulated.
            match n.AllElements with
            | Some xs   ->
                xs
            | None      ->
                // Collect all elements
                let rec go = function
                    | Tree.Leaf xs   ->
                        xs
                    | Tree.Node n   -> 
                        Array.collect (snd >> go) <| n.Children.Elements()
                go tree


