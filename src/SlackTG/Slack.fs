namespace SlackTG

module Slack =
    module OutboundTypes =
        type SlackText = | Plain of string | Markdown of string
        with member x.Text = match x with Plain s -> s | Markdown m -> m
    
        type Attachment = {
            Title : SlackText option
            PreText : SlackText option
            Text : SlackText option
            Fallback : string
            Image : System.Uri option
        }
        with 
            static member simple text = {Title = None; PreText = None; Text = Some <| Plain text; Fallback = text; Image = None} 
            static member nameValue (a : Attachment) =
                seq {
                    match a.Title with | Some s -> yield "title", s | None -> ()
                    match a.PreText with | Some s -> yield "pretext", s | None -> ()
                    match a.Text with | Some s -> yield "text", s | None -> () 
                    match a.Image with | Some i -> yield "image_url", (Plain (string i)) | None -> ()
                    yield "fallback", a.Fallback |> Plain
                }
            
        type ResponseType = | InChannel | Ephemeral
        with override x.ToString () = match x with | InChannel -> "in_channel" | Ephemeral -> "ephemeral"
        type SlackResponse = {
            Attachments : Attachment []
            ResponseType : ResponseType
        }
        with static member ofAttachments a = {ResponseType = InChannel; Attachments = a }
    
    module Commands =
        open OutboundTypes
        open Deckbrew

        type Command = 
            // /mtg cards color=green,blue set=SOI
            | Cards of args : Map<string, string list> 
            | Card of name : string
            | Help
        

        let (|Text|_|) (s :string) v = 
            if s = v then Some Text 
            else None
        let helpResponse = [| Attachment.simple "try /mtg card CARDNAME or /mtg cards type=creature. See https://deckbrew.com/api/ for full querying api" |] |> SlackResponse.ofAttachments
            
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
            [|  ["Error: "] @ err.Errors 
                |> String.concat "\n"
                |> Attachment.simple |] |> SlackResponse.ofAttachments
        
        let makeCardsResponse (cards : Types.CardModel.Card list) : SlackResponse = 
            [| cards |> List.map (fun c -> c.Name) |> String.concat "\n" |> Attachment.simple  |] |> SlackResponse.ofAttachments
        
        let makeCardResponse (card : Types.CardModel.Card) : SlackResponse =
            let firstEditionImage = card.Editions |> Array.tryLast |> Option.map (fun e -> e.ImageUrl |> System.Uri)
            [| {Attachment.simple (card.Name)  with Image = firstEditionImage } |] |> SlackResponse.ofAttachments

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
                | Card name -> 
                    async {
                        let! response = API.getCard name
                        match response with
                        | Choice2Of2 err -> return makeErrorResponse err
                        | Choice1Of2 card -> return makeCardResponse card
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
        let text = "Sorry, I didn't understand that request."

        [|
            {Attachment.simple text with 
                Title = Some <| Plain "Unknown Command" }
        |] |> SlackResponse.ofAttachments

    let getCommand (args : InboundTypes.SlackArgs) = args.command

    let handleSlackCommand = getCommand >> Commands.handleCommand
                