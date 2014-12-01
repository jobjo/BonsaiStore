namespace FSharp.BonsaiStore.Benchmarks
module Main =
    module R = FSharp.BonsaiStore.Benchmarks.Reports
    [<EntryPoint>]
    let main argv =
        R.countItemsBenchMark ()
        0
