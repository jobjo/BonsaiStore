namespace FSharp.BonsaiStore.Service
[<AutoOpen>]
module Interfaces =
    open FSharp.BonsaiStore
    

    /// Result of update operation
    type UpdateResult =
        | OK
        | Error of string

    /// Generic service interface.
    type Service<'T> =
        {
            // Get latest version.
            Get : unit -> 'T

            // Update latest version
            Modify : ('T -> 'T) -> Async<UpdateResult>
        }

    /// IStore service.
    type ServiceStore<'T> = 
        internal { Service : Service<IStore<'T>>}
        interface IStore<'T> with
            member this.Report f m r = this.Service.Get().Report f m r
            member this.Insert xs  = this.Service.Get().Insert xs
            member this.Filter f = this.Service.Get().Filter f
            member this.Map f = this.Service.Get().Map f