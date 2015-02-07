namespace FSharp.BonsaiStore
[<AutoOpen>]
module Prelude =
    open Microsoft.FSharp.Quotations

    /// Build default store.
    let buildDefaultStore = StoreBuilder.buildDefaultStore
    
    /// Build store.
    let buildStore conf = StoreBuilder.buildStore conf

    /// Create a report from a store.
    let report<'T,'R>  (store: IStore<'T>) 
                       (filter: Expr<'T -> bool>)
                       (map: 'T -> 'R) 
                       (reduce: 'R [] -> 'R) 
                       : 'R =
        store.Report filter map reduce

    /// Returns a subset of all items.
    let find<'T> (store: IStore<'T>) (filter: Expr<'T -> bool>) =
        report store filter (fun x -> [|x|]) Array.concat

    /// Returns all elements
    let elements<'T> (store: IStore<'T>) = find store <@ fun _ -> true @>

    /// Count number of elements.
    let count<'T> (store: IStore<'T>) filter = 
        report store filter (fun _ -> 1) Array.sum