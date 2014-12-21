namespace FSharp.BonsaiStore.Benchmarks
module Main =
    module R = FSharp.BonsaiStore.Benchmarks.DateReports
    open System
    open FSharp.Charting
    open System.Drawing 
    open System.Windows.Forms 

    open Utils

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
    
    module SR = SalesItemsReports
    module SB = FSharp.BonsaiStore.StoreBuilder
    open SalesItems

    [<EntryPoint>]
    [<STAThread>]
    let main argv =
        let results = SalesItemsReports.totalSalesBenchMark ()

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
