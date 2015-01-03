namespace FSharp.BonsaiStore.Service
module internal ServiceBuilder =

    /// Build a service given a default value.
    let buildService<'T> (def: 'T) =
        let value = ref def

        let getProc :  MailboxProcessor<AsyncReplyChannel<'T>> =
            MailboxProcessor.Start <| fun inbox ->
                let rec loop () =
                    async {
                        let! chn = inbox.Receive ()
                        chn.Reply !value
                        return! loop ()
                    }
                loop ()
        let updateProc : MailboxProcessor<('T -> 'T) * AsyncReplyChannel<UpdateResult>> =
            MailboxProcessor.Start <| fun inbox ->
                let rec loop () =
                    async {
                        let! f, chn = inbox.Receive ()
                        let v = !value
                        try
                            value := f v
                            chn.Reply (UpdateResult.OK)
                        with
                        | e ->
                            chn.Reply (UpdateResult.Error e.Message)
                        return! loop ()
                    }
                loop ()
        {
            Get = fun () -> getProc.PostAndReply id
            Modify = fun f -> 
                updateProc.PostAndAsyncReply (fun chn -> f, chn)
        }