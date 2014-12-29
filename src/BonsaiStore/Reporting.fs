namespace FSharp.BonsaiStore
module Reporting =
    open Microsoft.FSharp.Quotations

    /// Create a report from a store.
    let report<'T,'R>  (store: IStore<'T>) 
                       (filter: Expr<'T -> bool>) 
                       (empty: 'R) 
                       (map: 'T -> 'R) 
                       (reduce: 'R [] -> 'R) 
                       : 'R =
        store.Report filter { Empty = empty; Map = map; Reduce = reduce}

    /// Returns a subset of all items.
    let find<'T> (store: IStore<'T>) (filter: Expr<'T -> bool>) =
        report store filter [||] (fun x -> [|x|]) Array.concat