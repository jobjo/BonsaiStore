namespace FSharp.BonsaiStore.Service

[<AutoOpen>]
module Prelude =
    open FSharp.BonsaiStore
    module SB = ServiceBuilder

    /// Builds a new service given an IStore object.
    let fromStore (store: IStore<'T>) : ServiceStore<'T> = 
        { Service = SB.buildService store}

    /// Builds a service given a sequence of items.
    let buildService conf (items: 'T []) : ServiceStore<'T> =
        fromStore <| buildStore conf items

    /// Builds a service given a sequence of items.
    let buildDefaultService (items: 'T []) : ServiceStore<'T> =
        fromStore <| buildDefaultStore items

    /// Modify a service.
    let modify (store: ServiceStore<'T>) f =
        store.Service.Modify f

    /// Insert elements.
    let insert (store: ServiceStore<'T>) xs =
        store.Service.Modify (fun s -> s.Insert xs)

    /// Filter elements.
    let filter (store: ServiceStore<'T>) f = 
        store.Service.Modify (fun s -> s.Filter f)

    /// Map over elements.
    let map (store: ServiceStore<'T>) f =
        store.Service.Modify (fun s -> s.Map f)
        

