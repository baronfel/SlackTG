// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Suave
open SlackTG
open System.Net
open System
open System.Threading
open Logary
open Suave.Logging
open Logary.Message
open Logary.Configuration.Config
open Logary.Targets
open Logary.Adapters.Facade

[<EntryPoint>]
let main argv = 
    let cts = new CancellationTokenSource()
    let logary =
        withLogaryManager "SlackTG" (
            withTargets [
                LiterateConsole.create LiterateConsole.empty "console"
                Debugger.create Debugger.empty "debugger"
            ] 
            >> withRules [
                Rule.createForTarget "console"
                Rule.createForTarget "debugger"
            ]
        ) |> Hopac.Hopac.run
        
    let config = 
        { defaultConfig with
            listenTimeout = TimeSpan.FromSeconds 1. 
            cancellationToken = cts.Token }

    LogaryFacadeAdapter.initialise<Suave.Logging.Logger> logary

    let started, listening = startWebServerAsync config SlackTG.Suave.app
    Async.Start(listening, cts.Token)
    Console.ReadLine() |> ignore
    cts.Cancel()
    (logary.getLogger (PointName [|"root"|])).info (eventX "shutting down")
    exit 0