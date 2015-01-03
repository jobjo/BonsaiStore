namespace FSharp.BonsaiStore.Benchmarks
module Main =
    module R = FSharp.BonsaiStore.Benchmarks.DateBenchmarks
    open FSharp.Charting
    open System.Windows.Forms 
    open Utils

    module SR = SalesItemsBenchmarks
    module SB = FSharp.BonsaiStore.StoreBuilder

    let chartBenchmark (groups: list<GroupResult>) =
        [
            for group in groups do
                yield 
                    [
                        for sr in group.SampleResults do
                            yield sr.Name, sr.Time
                    ]
                    |> Chart.Column
                    |> Chart.WithTitle group.Name
        ]

    [<EntryPoint>]
    let main argv =


        let results = DateBenchmarks.countItemsBenchMark () // SalesItemsReports.benchmark ()

        Utils.showBenchmark results
        
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault false

        let chart =
            results
            |> chartBenchmark
            |> Chart.Rows
        
        let form = chart.ShowChart()
        form.Height <- 800
        form.Show()
        Application.Run(form);
        0
