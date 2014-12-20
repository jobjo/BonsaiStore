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


    // Return current used memory in MBs.
    let getCurrentMemory () =
        let mem = System.GC.GetTotalMemory(true)
        System.Math.Round((float mem) / (System.Math.Pow(2.,20.)), 1)

    let rec repeate n f = if n <= 0 then () else f () ; repeate (n-1) f
    let testCase f () = ignore <| f ()
    type Sample =
        {
            Name : string
            Function : unit -> unit
        }
    type Group = 
        {
            Name : string
            Samples : list<Sample>
        }

    type SampleResult =
        {
            Name : string
            Time : float
            RelativeTime : float
        }
    type GroupResult =
        {
            Name : string
            SampleResults : list<SampleResult>
        }

    let benchmark n (groups: list<string * list<string * (unit -> unit)>>) : list<GroupResult> =
        [
            for _ in [1 .. n] do
                for (group,samples) in groups do
                    for (name, sample) in samples do
                        let t = snd <| time sample
                        yield group, name, t
        ]
        |> Seq.groupBy (fun (gn,_,_) -> gn)
        |> Seq.map (fun (gn, rs) ->
            // Group samples by name
            let groupRes =
                rs
                |> Seq.groupBy (fun (_,n,_) -> n)
                |> Seq.map (fun (n, rs) ->
                    let avg = Seq.averageBy (fun (_,_,t) -> t) rs
                    (n, avg)
                )
            let minTime = snd <| Seq.minBy snd groupRes
            let sampleRes =
                groupRes
                |> Seq.map (fun (name, avg) ->
                    {
                        Name = name
                        Time = System.Math.Round(avg, 2)
                        RelativeTime = System.Math.Round(avg / minTime, 2)
                    }
                )
            {Name = gn ; SampleResults = List.ofSeq sampleRes}
        )
        |> List.ofSeq



    let showBenchmark (groups: list<GroupResult>) =
        
        let showFloat (f: float) =
            f.ToString("#.00", System.Globalization.CultureInfo.InvariantCulture)
        
        let maxBy f =
            groups
            |> Seq.collect (fun group -> Seq.map f group.SampleResults)
            |> Seq.max
        let maxNameLen = 
            let m1 = maxBy (fun sample ->  sample.Name.Length)
            let m2 = Seq.max <| Seq.map (fun g -> g.Name.Length) groups
            max m1 m2
        let maxTimeLen = maxBy (fun sample -> sample.Time |> showFloat |> Seq.length)
        let maxRelTimeLen = maxBy (fun sample -> sample.RelativeTime |> showFloat |> Seq.length)

        let nameColLen = maxNameLen + 4
        let timeColLen = maxTimeLen + 4
        let timeRelColLen = maxRelTimeLen + 4
        let tableLength = nameColLen + timeColLen + timeRelColLen
        
        let replicate n c = System.String.Join ("",List.replicate n c)

        let printTableHeader () = printfn "%s" <| replicate tableLength "-"

        let left w (s: string) =
            let space = replicate (w - s.Length) " "
            printf "%s%s" s space

        let right w (s: string) =
            let space = replicate (w - s.Length) " "
            printf "%s%s" space s

        for group in groups do
            printfn ""
            printTableHeader ()
            left nameColLen group.Name
            right timeColLen "Time"
            right timeRelColLen "Rel-Time"
            printfn ""
            printTableHeader ()
            for sample in group.SampleResults do
                left nameColLen sample.Name
                right timeColLen <| showFloat sample.Time
                right timeRelColLen <| showFloat sample.RelativeTime
                printfn ""
            printfn "\n"

