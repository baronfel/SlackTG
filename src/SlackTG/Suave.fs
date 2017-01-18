namespace SlackTG

module Parsing = 
    open FSharpx.Option
    open Slack
    open InboundTypes
    open Commands
    open FSharp.Data
    open OutboundTypes 

    let writeAttachment attachment =  
        let props = Attachment.nameValue attachment
        let markdownProp = [ "mrkdwn_in", props |> Array.ofSeq |> Array.filter (snd >> (fun v -> match v with | Markdown _ -> true | _ -> false)) |> Array.map (fst >> JsonValue.Parse) |> JsonValue.Array ]
        let initial = 
            props
            |> Seq.map (fun (name,value) -> name, (value.Text |> JsonValue.String) )
                
        Seq.append initial markdownProp
        |> Array.ofSeq
        |> JsonValue.Record

    let writeSlackResponse (slackResponse : SlackResponse) =  
        [| "attachments", slackResponse.Attachments |> Array.ofList |> Array.map writeAttachment |> JsonValue.Array
           "response_type", string slackResponse.ResponseType |> JsonValue.String |]
        |> JsonValue.Record

    let tryParseUri uri = 
        match System.Uri.TryCreate(uri, System.UriKind.Absolute) with
        | true, uri' -> Some uri'
        | _ -> None

module Suave =

    open Suave
    open Suave.Successful  
    open Suave.Filters
    open Suave.Operators
    open Suave.Writers
    open Suave.Json

    open Slack.InboundTypes
    open Parsing
       
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
            let! command = findInForm "command" (fun c -> c.TrimStart('/').ToLowerInvariant())
            let! args = findInForm "text" (fun argString -> argString.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)
            let! response_url = findInForm "response_url" tryParseUri |> Option.bind id
            
            let team = { id = team_id; domain = team_domain }
            let channel = { ChannelInfo.id = channel_id; name = channel_name }
            let user = { UserInfo.id = user_id; name = user_name }
            //let! command = parseCommand command args
            return {token = token; team = team; channel = channel; user = user; command = command; args = args; response_url = response_url}
        }

    let asJson item : WebPart = 
        let json = Parsing.writeSlackResponse item
        json |> string |> OK
        >=> setMimeType "application/json;charset=utf-8" 

    let slackApp (commands : Map<string, Slack.SlackCommand>) (request : HttpRequest) : WebPart = 
        let helpResponse = 
            let usages = commands |> Map.toSeq |> Seq.map (snd >> fun c -> c.usage |> Slack.OutboundTypes.Attachment.simple) |> Seq.toList
            { Slack.OutboundTypes.SlackResponse.ofAttachments (Slack.confusedResponse :: usages) with ResponseType = Slack.OutboundTypes.Ephemeral }
        let slackRequest = extractFormFields request
        let slackResponse =
            match slackRequest with
            | Some slackMessage -> 
                match commands |> Map.tryFind slackMessage.command with
                | Some command -> 
                    command.handler slackMessage.args |> Async.RunSynchronously
                | None -> helpResponse
            | None -> helpResponse
                
        asJson slackResponse
    
    let cardCommand : Slack.SlackCommand = { name = "card" 
                                             usage = "card CARDNAME" 
                                             handler = Slack.Commands.handleCard }
    let cardsCommand : Slack.SlackCommand = { name = "cards"
                                              usage = "cards FILTER=VALUE"
                                              handler = Slack.Commands.handleCards }
    let normalCommands = 
        let actualCommands = [
            cardCommand
            cardsCommand
        ]

        let makeHelpCommand (cmds : Slack.SlackCommand list) : Slack.SlackCommand =
            let usage = "Usage:" :: (cmds |> List.map (fun c -> c.usage)) |> String.concat "\n"
            { name = "help"
              usage = usage
              handler = fun _ -> async {return Slack.OutboundTypes.SlackResponse.ofAttachments [ Slack.OutboundTypes.Attachment.simple usage ] } }

        actualCommands
        |> List.fold (fun m c -> m |> Map.add c.name c) Map.empty
        |> Map.add "help" (makeHelpCommand actualCommands)

    let app : WebPart = 
        
        choose [
            GET >=> path "/" >=> OK "alive"
            POST >=> path "/message" >=> request (slackApp normalCommands)
        ]
