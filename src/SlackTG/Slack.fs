namespace SlackTG

module Slack =
    module OutboundTypes =
        type SlackText = | Plain of string | Markdown of string
        with member x.Text = match x with Plain s -> s | Markdown m -> m
    
        type Attachment = {
            Title : SlackText option
            PreText : SlackText option
            Text : SlackText option
        }
        with 
            static member simple text = {Title = None; PreText = None; Text = Some <| Plain text} 
            static member nameValue (a : Attachment) =
                seq {
                    match a.Title with | Some s -> yield "title", s | None -> ()
                    match a.PreText with | Some s -> yield "pretext", s | None -> ()
                    match a.Text with | Some s -> yield "text", s | None -> () 
                }
        type SlackResponse = Attachment []
    
    module Commands =
        open OutboundTypes
        open Deckbrew

        type Command = 
            // /mtg cards color=green,blue set=SOI
            | Cards of args : Map<string, string[]> 
            | Help

        let helpResponse = [| Attachment.simple "try /mtg cards color=blue,green" |]
            
        let cardsArgsToGetParams args = List.empty

        let makeErrorResponse (err : Deckbrew.Types.Error) : SlackResponse = 
            [|  ["Error: "] @ err.Errors 
                |> String.concat "\n"
                |> Attachment.simple |]
        
        let makeCardsResponse (cards : Types.CardModel.Card list) : SlackResponse = 
            [| cards |> List.map (fun c -> c.Name) |> String.concat "\n" |> Attachment.simple  |]

        let handleCommand : Command -> Async<SlackResponse> = 
            fun c -> 
                match c with
                | Help -> async { return helpResponse }
                | Cards args -> 
                    async {
                        let! response = args |> cardsArgsToGetParams |> API.getCards
                        match response with
                        | Choice2Of2 err -> return makeErrorResponse err 
                        | Choice1Of2 cards ->  return makeCardsResponse cards.Payload
                    }

    module InboundTypes = 
        open Commands 

        type TeamInfo = {
            id : string
            domain : string
        }
        type ChannelInfo = {
            id : string
            name : string
        }

        type UserInfo = {
            id : string
            name : string
        }
        
        type SlackArgs = {
            token : string
            team : TeamInfo
            channel : ChannelInfo
            user : UserInfo
            command : Command
            response_url : System.Uri
        }

    module Parsing = 
        open FSharpx.Option
        open InboundTypes
        open Commands
        open FSharp.Data
        open OutboundTypes 

        let parseCardsArgs args = Map.empty

        let parseCommand commandName args = 
            match commandName with
            | "mtg" -> 
                match args with
                | "help"::_ -> Some Help
                | "cards"::cardArgs -> 
                    let args' = parseCardsArgs cardArgs
                    Some <| Cards args'
                | _ -> None
            | _ -> None

        let tryParseUri uri = 
            match System.Uri.TryCreate(uri, System.UriKind.Absolute) with
            | true, uri' -> Some uri'
            | _ -> None

        let parseSlackArgs (formPost : Map<string,string>) : SlackArgs option = 
            let inline findInForm k = Map.tryFind k formPost
            maybe {
                let! token = findInForm "token"
                let! team_id = findInForm "team_id"
                let! team_domain = findInForm "team_domain"
                let! channel_id = findInForm "channel_id"
                let! channel_name = findInForm "channel_name"
                let! user_id = findInForm "user_id"
                let! user_name = findInForm "user_name"
                let! command = findInForm "command" |> Option.map (fun c -> c.TrimStart('/').ToLowerInvariant())
                let! args = findInForm "text" |> Option.map (fun argString -> argString.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)
                let! response_url = findInForm "response_url" |> Option.bind tryParseUri
            
                let team = { id = team_id; domain = team_domain }
                let channel = { ChannelInfo.id = channel_id; name = channel_name }
                let user = { UserInfo.id = user_id; name = user_name }
                let! command = parseCommand command args
                return {token = token; team = team; channel = channel; user = user; command = command; response_url = response_url}
            }

        let writeAttachment attachment =  
            let props = Attachment.nameValue attachment
            let markdownProp = [ "mrkdwn_in", props |> Array.ofSeq |> Array.filter (snd >> (fun v -> match v with | Markdown _ -> true | _ -> false)) |> Array.map (fst >> JsonValue.Parse) |> JsonValue.Array ]
            let initial = 
                props
                |> Seq.map (fun (name,value) -> name, (value.Text |> JsonValue.String) )
                
            Seq.append initial markdownProp
            |> Array.ofSeq
            |> JsonValue.Record

        let writeSlackResponse slackResponse =  
            [| ("Attachments", slackResponse |> Array.map writeAttachment |> JsonValue.Array) |]
            |> JsonValue.Record
    
    open OutboundTypes
    open FSharpx.Option

    let confusedResponse : SlackResponse = 
        [|
            {Title = Some <| Plain "Unknown command"; PreText = None; Text = Some <| Plain "Sorry, I didn't understand that request."}
        |]

    let handler : Map<string, string> -> Async<string> = 
        fun formPost -> 
                formPost 
                |> Parsing.parseSlackArgs
                |> Option.map (fun a -> a.command)
                |> Option.map Commands.handleCommand
                |> getOrElse (async { return confusedResponse })
                |> FSharpx.Async.map Parsing.writeSlackResponse
                |> FSharpx.Async.map string
                