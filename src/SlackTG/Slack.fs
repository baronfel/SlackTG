namespace SlackTG

module Slack =
    module OutboundTypes =
        open Chiron
        open Chiron.Operators

        type SlackText = | Plain of string | Markdown of string
        with member x.Text = match x with Plain s -> s | Markdown m -> m
             static member ToJson (s : SlackText) = 
                match s with
                | Plain s -> Json.Optic.set Json.String_ s
                | Markdown s -> Json.Optic.set Json.String_ s
             static member IsMarkdownString = function | Plain _ -> false | Markdown _ -> true
        let uriToJson (u : System.Uri) = String (string u)

        type Attachment = {
            Title : SlackText option
            PreText : SlackText option
            Text : SlackText option
            Fallback : string
            Image : System.Uri option
        }
        with
            static member ToJson (a : Attachment) = 
                 let markdownFields = 
                    seq { if defaultArg (Option.map SlackText.IsMarkdownString a.Title) false then yield "title"
                          if defaultArg (Option.map SlackText.IsMarkdownString a.PreText) false then yield "pretext"
                          if defaultArg (Option.map SlackText.IsMarkdownString a.Text) false then yield "text" } |> Seq.toArray
                 Json.write "fallback" a.Fallback
                 *> Json.writeUnlessDefault "title" None a.Title
                 *> Json.writeUnlessDefault "pretext" None a.PreText
                 *> Json.writeUnlessDefault "text" None a.Text
                 *> json {
                        match a.Image with
                        | Some uri ->
                            do! Json.write "image" (string uri) 
                        | None -> ()
                    }
                 *> Json.writeUnlessDefault "mrkdwn_in" [||] markdownFields
            static member simple text = {Title = None; PreText = None; Text = Some <| Plain text; Fallback = text; Image = None} 
            
        type ResponseType = | InChannel | Ephemeral
        with static member ToJson r = 
                match r with 
                | InChannel -> Json.Optic.set Json.String_ "in_channel"
                | Ephemeral -> Json.Optic.set Json.String_ "ephemeral"
        type SlackResponse = {
            Attachments : Attachment list
            ResponseType : ResponseType
        }
        with static member ofAttachments a = {ResponseType = InChannel; Attachments = a }
             static member ToJson (r : SlackResponse) = 
                Json.write "response_type" r.ResponseType 
                *> Json.write "attachments" r.Attachments

    module Commands =
        open OutboundTypes
        open Deckbrew

        type Command = 
            // /mtg cards color=green,blue set=SOI
            | Cards of args : Map<string, string list> 
            | Card of name : string
            | Help

        let makeCardName (parts : string list) = parts |> String.concat " "

        let parseCardsArgs (args : string list) =
            args
            |> List.map (String.split '=' >> fun l -> List.item 0 l,  List.item 1 l)
            |> List.fold (fun m (k,v) ->
                match m |> Map.tryFind k with
                | None -> m |> Map.add k [v]
                | Some vs -> m |> Map.add k (v :: vs)
            ) Map.empty

        let parseCommand commandName args = 
            match commandName with
            | "mtg" -> 
                match args with
                | "help"::_ -> Some Help
                | "card"::nameparts -> Some <| Card (makeCardName nameparts)
                | "cards"::cardArgs -> 
                    let args' = parseCardsArgs cardArgs
                    Some <| Cards args'
                | _ -> None
            | _ -> None

        let (|Text|_|) (s :string) v = 
            if s = v then Some Text 
            else None
        let helpResponse = [ Attachment.simple "try /mtg card CARDNAME or /mtg cards type=creature. See https://deckbrew.com/api/ for full querying api" ] |> SlackResponse.ofAttachments
            
        let cardsArgsToGetParams (args : Map<string, string list>) =
            let toCardArgsBasedOnKey (k : string) (vs : string list) =
                match k with
                | Text "type" -> List.map (Types.CT >> API.Type) vs 
                | Text "subtype" -> vs |> List.choose Types.SubType.TryParse |> List.map API.SubType
                | Text "supertype" -> vs |> List.choose Types.SuperType.TryParse |> List.map API.SuperType
                | Text "name" -> vs |> List.map API.Name
                | Text "oracle" -> vs |> List.map API.Oracle
                | Text "set" -> vs |> List.map API.Set
                | Text "rarity" -> vs |> List.choose Types.Rarity.TryParse |> List.map API.Rarity
                | Text "color" -> vs |> List.choose Types.Color.TryParse |> List.map API.Color
                | Text "multicolor" -> vs |> List.choose (bool.TryParse >> function | true,v -> Some v | _ -> None) |> List.fold (&&) true |> API.Multicolor |> List.singleton
                | Text "multiverseid" | Text "m" -> vs |> List.map API.MultiverseId
                | Text "format" -> vs |> List.choose Types.Format.TryParse |> List.map API.Format
                | Text "status" -> vs |> List.choose Types.Status.TryParse |> List.map API.Status
                | _ -> []

            args
            |> Map.fold (fun list k vs -> toCardArgsBasedOnKey k vs @ list) List.empty

        let makeErrorResponse (err : Deckbrew.Types.Error) : SlackResponse = 
            [ ["Error: "] @ err.Errors 
              |> String.concat "\n"
              |> Attachment.simple ] |> SlackResponse.ofAttachments
        
        let makeCardsResponse (cards : Types.CardModel.Card list) : SlackResponse = 
            [ cards |> List.map (fun c -> c.Name) |> String.concat "\n" |> Attachment.simple ] |> SlackResponse.ofAttachments
        
        let makeCardResponse (card : Types.CardModel.Card) : SlackResponse =
            let firstEditionImage = card.Editions |> Array.tryLast |> Option.map (fun e -> e.ImageUrl |> System.Uri)
            [ {Attachment.simple (card.Name)  with Image = firstEditionImage } ] |> SlackResponse.ofAttachments
        
        let handleCard args : Async<SlackResponse> = 
            async {
                let! response = API.getCard (makeCardName args)
                match response with
                | Choice1Of2 card -> return makeCardResponse card
                | Choice2Of2 err -> return makeErrorResponse err
            }
        
        let handleCards args : Async<SlackResponse> = 
            async {
                let! response = args |> parseCardsArgs |> cardsArgsToGetParams |> API.getCards
                match response with
                | Choice2Of2 err -> return makeErrorResponse err 
                | Choice1Of2 cards ->  return makeCardsResponse cards.Payload
            }
        
    module InboundTypes = 
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
            command : string
            args : string list
            response_url : System.Uri
        }

    open OutboundTypes

    type SlackCommand = {
        name : string
        usage : string
        handler : string list -> Async<OutboundTypes.SlackResponse>
    }

    let confusedResponse = 
        let text = "Sorry, I didn't understand that request."
        { Attachment.simple text with 
                Title = Some <| Plain "Unknown Command" }
    let cardCommand : SlackCommand = { name = "card" 
                                       usage = "card CARDNAME" 
                                       handler = Commands.handleCard }
    let cardsCommand : SlackCommand = { name = "cards"
                                        usage = "cards FILTER=VALUE"
                                        handler = Commands.handleCards }
    let normalCommandSet = 
        let actualCommands = [
            cardCommand
            cardsCommand
        ]

        let makeHelpCommand (cmds : SlackCommand list) : SlackCommand =
            let usage = "Usage:" :: (cmds |> List.map (fun c -> c.usage)) |> String.concat "\n"
            { name = "help"
              usage = usage
              handler = fun _ -> async {return SlackResponse.ofAttachments [ Attachment.simple usage ] } }

        actualCommands
        |> List.fold (fun m c -> m |> Map.add c.name c) Map.empty
        |> Map.add "help" (makeHelpCommand actualCommands)

                