// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Suave
open SlackTG
open System.Net
open System
open System.Threading

[<EntryPoint>]
let main [| port |] = 
    let logger = 
        { new Suave.Logging.Logger with 
            member x.Log level lineFormat = 
                let line = lineFormat()
                printfn "[%A] %s: %s" line.level (DateTime.UtcNow.ToString("o")) line.message        }
    let cts = new CancellationTokenSource()

    let config = 
        { defaultConfig with
            bindings = 
                [ 
                    HttpBinding.mk HTTP (IPAddress.Parse("0.0.0.0")) (uint16 port) 
                    HttpBinding.mk HTTP IPAddress.Any (uint16 port) 
                    HttpBinding.mk HTTP IPAddress.Loopback (uint16 port)
                    HttpBinding.mk HTTP IPAddress.IPv6Any (uint16 port)
                    HttpBinding.mk HTTP IPAddress.IPv6Loopback (uint16 port)
                    //TODO: https binding with custom cert
                ] 
            listenTimeout = TimeSpan.FromSeconds 1. 
            logger = Suave.Logging.Loggers.saneDefaultsFor Logging.LogLevel.Verbose
            cancellationToken = cts.Token }
    printfn "starting server at port %s" port
    startWebServer config SlackTG.Suave.app
    Console.ReadLine() |> ignore
    cts.Cancel()
    printfn "exiting server..." 
    exit 0