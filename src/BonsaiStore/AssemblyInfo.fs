namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("BonsaiStore")>]
[<assembly: AssemblyProductAttribute("BonsaiStore")>]
[<assembly: AssemblyDescriptionAttribute("BonsaiStore enables creation of data stores with efficient querying/filtering via an F# interface.")>]
[<assembly: AssemblyVersionAttribute("0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1"
