namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("SlackTG.Host")>]
[<assembly: AssemblyProductAttribute("SlackTG")>]
[<assembly: AssemblyDescriptionAttribute("A Slack slash command bot for querying the magic trading card game database")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
    let [<Literal>] InformationalVersion = "0.0.1"
