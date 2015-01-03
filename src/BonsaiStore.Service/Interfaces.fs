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
    type ServiceStore<'T> = Service<IStore<'T>>