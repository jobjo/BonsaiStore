namespace FSharp.BonsaiStore.Internal

module Tree =
    
    open FSharp.Collections.RangeMap
    module P = Utils.Printer
    let private (!<) = P.(!<)

    /// A node has a level attribute and a map to child nodes.
    type Node<'K, 'L,'T> =
        {
            /// Filter type
            Level : 'L
            /// Child nodes
            Children : IRangeMap<'K, Tree<'K,'L,'T>>
        }

    /// A Tree is either a node or a leaf.
    and Tree<'K,'L,'T> =
        | Node of Node<'K,'L,'T>
        | Leaf of 'T

    /// Functor instance for tree.
    let rec map f = function
        | Node n -> 
            Node {
                Level = n.Level
                Children =  n.Children.Map (fun t -> map f t) 
            }
        | Leaf x    -> 
            Leaf <| f x

    /// Pretty prints a tree.
    let showTree<'K, 'L,'T when 'K : comparison> =
        let rec go (tree: Tree<'K, 'L,'T>) =
            match tree with
            | Leaf (x: 'T)    ->
                P.print (string (box x))
            | Node (node: Node<'K, 'L,'T>) ->
                !< [
                    yield P.print (sprintf "[%s]" <| node.Level.ToString())
                    for (k,v) in node.Children.Elements() do
                        yield P.print (sprintf "%s" (k.ToString()))
                        yield P.indent <| go v
                ]
        go >> P.run

    /// Configurations for building a tree.    
    type IBuildTreeConfiguration =
        abstract BuildMap<'K,'V when 'K : comparison> : seq<'K * 'V> -> IRangeMap<'K,'V>

    /// Default tree configuration.
    let defaultBuildTreeConfiguration =
        { new IBuildTreeConfiguration with 
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
                Tree.Node {Level = level; Children = children}
        go (Array.ofSeq elements) levels

    /// Extract all elements from a tree.
    let elements (tree: Tree<'K,'L,'T>) =
        let rec go = function
            | Tree.Leaf x  -> [x]
            | Tree.Node n   -> List.collect (snd >> go) <| n.Children.Elements()
        go tree


