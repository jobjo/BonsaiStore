namespace FSharp.BonsaiStore.Service

module Store =
    open FSharp.BonsaiStore
    module SB = ServiceBuilder

    let buildService (store: IStore<'T>) : ServiceStore<'T> = 
        SB.buildService store

    let modify (store: ServiceStore<'T>) f =
        store.Modify f

    let report (store: Service<IStore<'T>>) f m r =
        let s = store.Get()
        s.Report f m r

    let insert (store: ServiceStore<'T>) xs =
        store.Modify (fun s -> s.Insert xs)

    let filter (store: ServiceStore<'T>) f = 
        store.Modify (fun s -> s.Filter f)

    let map (store: ServiceStore<'T>) f =
        store.Modify (fun s -> s.Map f)
        

