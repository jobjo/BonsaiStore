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
    
    [<EntryPoint>]
    [<STAThread>]
    let main argv =

        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault false
        SalesItemsReports.totalSalesBenchMark()
        |> ignore

        let chart =
            //R.countItemsBenchMark ()
            SalesItemsReports.totalSalesBenchMark ()
            |> chartBenchmark
            |> Chart.Rows
        
        let form = chart.ShowChart()
        form.Height <- 800
        form.Show()
        Application.Run(form);
        0
