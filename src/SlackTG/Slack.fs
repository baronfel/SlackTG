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

    open OutboundTypes

    let confusedResponse : SlackResponse = 
        [|
            {Title = Some <| Plain "Unknown command"; PreText = None; Text = Some <| Plain "Sorry, I didn't understand that request."}
        |]

    let getCommand (args : InboundTypes.SlackArgs) = args.command

    let handleSlackCommand = getCommand >> Commands.handleCommand
                