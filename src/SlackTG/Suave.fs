namespace SlackTG

module Suave =

    open Suave
    open Suave.Successful  
    open Suave.Filters
    open Suave.Operators
    open Suave.Writers
    open Suave.Json

    open Slack.InboundTypes

    let tryParseUri uri = 
        match System.Uri.TryCreate(uri, System.UriKind.Absolute) with
        | true, uri' -> Some uri'
        | _ -> None
       
    let extractFormFields (request: HttpRequest) = 
        let map = request.form |> Map
        let inline findInForm k (f : string -> 'b) = 
            FSharpx.Option.maybe {
                let! result = Map.tryFind k map
                match result with
                | None -> return! None
                | Some r -> return f r
            }

        FSharpx.Option.maybe {
            let! token = findInForm "token" id
            let! team_id = findInForm "team_id" id
            let! team_domain = findInForm "team_domain" id
            let! channel_id = findInForm "channel_id" id
            let! channel_name = findInForm "channel_name" id
            let! user_id = findInForm "user_id" id
            let! user_name = findInForm "user_name" id
            let! _ = findInForm "command" (fun c -> c.TrimStart('/').ToLowerInvariant())
            let! args = findInForm "text" (fun argString -> argString.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)
            let! response_url = findInForm "response_url" tryParseUri |> Option.bind id
            
            let team = { id = team_id; domain = team_domain }
            let channel = { ChannelInfo.id = channel_id; name = channel_name }
            let user = { UserInfo.id = user_id; name = user_name }
            let! (command, args) = 
                match args with
                | [] -> None
                | [cmd] -> Some (cmd, [])
                | cmd::args -> Some (cmd, args)
            return {token = token; team = team; channel = channel; user = user; command = command; args = args; response_url = response_url}
        }

    let asJson item : WebPart = 
        let json = Chiron.Mapping.Json.serialize item |> Chiron.Formatting.Json.formatWith Chiron.Formatting.JsonFormattingOptions.Pretty
        OK json
        >=> setMimeType "application/json;charset=utf-8" 

    let slackApp (commands : Map<string, Slack.SlackCommand>) (request : HttpRequest) : WebPart = 
        let helpResponse command = 
            let usage = commands |> Map.find "help" |> fun c -> c.usage |> Slack.OutboundTypes.Attachment.simple
            { Slack.OutboundTypes.SlackResponse.ofAttachments [MTG.confusedResponse command; usage] with ResponseType = Slack.OutboundTypes.Ephemeral }
        let slackRequest = extractFormFields request
        let slackResponse =
            match slackRequest with
            | Some slackMessage -> 
                match commands |> Map.tryFind slackMessage.command with
                | Some command -> 
                    command.handler slackMessage.args |> Async.RunSynchronously
                | None -> helpResponse slackMessage.command
            | None -> { Slack.OutboundTypes.SlackResponse.ofAttachments [Slack.OutboundTypes.Attachment.simple "sorry, your message was malformed"] with ResponseType = Slack.OutboundTypes.Ephemeral} 
                
        asJson slackResponse

    let app : WebPart = 
        
        choose [
            GET >=> path "/" >=> OK "alive"
            POST >=> path "/message" >=> request (slackApp MTG.normalCommandSet)
        ]
