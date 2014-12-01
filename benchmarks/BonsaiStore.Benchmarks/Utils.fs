namespace FSharp.BonsaiStore.Benchmarks

module Utils =

    // Measure time
    let time f =
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start();
        let x = f ()
        sw.Stop();
        let ts = sw.Elapsed;
        x,  ts.TotalMilliseconds

    let rec repeate n f = if n <= 0 then () else f () ; repeate (n-1) f

    let testCase f () = ignore <| f ()

    let benchmark n fs =
        [
            for _ in [1 .. n] do
                for name, f in fs do
                    let t = snd <| time f
                    yield name, t
        ]
        |> Seq.groupBy fst
        |> Seq.map (fun (name, ns) ->
            let avg = Seq.averageBy snd ns
            (name, System.Math.Round(avg, 2))
        )

    // Return current used memory in MBs.
    let getCurrentMemory () =
        let mem = System.GC.GetTotalMemory(true)
        System.Math.Round((float mem) / (System.Math.Pow(2.,20.)), 1)