namespace FSharp.BonsaiStore.Internal

module Tree =

    open FSharp.Collections.RangeMap
    module P = Utils.Printer

    /// Level of the tree.
    type Level = int

    /// Key
    type Key = int

    let private (!<) = P.(!<)

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

    /// Empty tree
    let empty<'T> : Tree<'T> = Leaf [||]

    let isEmptyLeaf = function
        | Leaf [||] -> true
        | _         -> false

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
    let buildTree (conf: IBuildTreeConfiguration) 
                  (levels: list<Level * ('T -> Key)>) 
                  (elements : seq<'T>)  =
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
        | Leaf xs   ->
            xs
        | Node n    ->
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

    /// Height of tree.
    let rec height = function
        | Leaf _ -> 
            1
        | Node n ->
            1 + Array.max (Array.map (snd >> height) <| n.Children.Elements())

    /// Number of nodes.
    let rec numNodes = function
        | Leaf _ -> 
            1
        | Node n ->
            let sub =
                n.Children.Elements()
                |> Array.map (snd >> numNodes)
                |> Array.sum
            1 + sub

    /// Builds a tree from a list of levels and elements.
    let insert  (conf: IBuildTreeConfiguration)
                (levels: list<Level * ('T -> Key)>) 
                (tree: Tree<'T>) 
                (items : seq<'T>) 
                : Tree<'T> =
        let rec go items tree = function
            | []                        ->
                match tree with
                | Leaf xs   -> 
                    Leaf <| Array.append items xs
                | Node n    ->
                    failwith "Levels not matching."
            | (level,toKey) :: nextLevels as levels ->
                match tree with
                | Node n  when n.Level = level  ->
                    // Generate all modified child trees.
                    let modChs =
                        items
                        |> Seq.groupBy toKey
                        |> Seq.map (fun (key, items) ->
                            let items = Array.ofSeq items
                            let tree =
                                match n.Children.Lookup key with
                                | Some tree    ->
                                    (go items tree nextLevels)
                                | None      ->
                                    buildTree conf nextLevels items
                            key, tree
                        )
                    // Replaces new old children.
                    let chs =
                        (n.Children, modChs) 
                        ||> Seq.fold (fun map (k,v) -> map.Insert k v)
                    // Appends to cached collection if exists.
                    let items = Option.map (Array.append items) n.AllElements
                    Node  {n with  AllElements = items;  Children = chs }
                | Leaf oldItems                 ->
                    buildTree conf levels (Array.append items oldItems)
                | _                             ->
                    failwith "Levels not matching."
        go (Array.ofSeq items) tree levels

    /// Builds a tree from a list of levels and elements.
    let filter  (pred: 'T -> bool)
                (tree: Tree<'T>)
                : Tree<'T> =
        let rec go = function
            | Leaf xs   -> 
                Leaf <| Array.filter pred xs
            | Node n    ->
                let chs =
                    // Filter all children
                    let chs = n.Children.Map go
                    // Collect keys of empty children.
                    let ks =
                        [ for (k,tree) in chs.Elements() do
                            if isEmptyLeaf tree then yield k else () ]
                    // Prune the tree by removing all empty children.
                    Seq.fold (fun (t: IRangeMap<Key,_>) k ->  t.Remove k) chs ks
                if chs.Elements().Length = 0 then
                    Leaf [||]
                else
                    Node
                        { n with 
                            Children = chs
                            AllElements = Option.map (Array.filter pred) n.AllElements }
        go tree


