namespace SlackTG

module Async =  
    open System.Threading.Tasks

    type AsyncBuilder with
        member x.Bind (t: Task<_>, f : 'a -> Async<_>) = async.Bind(Async.AwaitTask t, f)

module mtgio = 
    open HttpFs
    open HttpFs.Client
    open System
    open Hopac
    open Chiron
    open Chiron.Parsing
    open Chiron.Formatting
    open Chiron.Operators
    open Serialization

    let rooturl = Uri "https://api.magicthegathering.io/v1"

    type Error = 
      { error : string } 
      with 
        static member FromJson (_ : Error) = 
            fun (e : string) -> { error = e; } 
            <!> Json.read "error"

    
    type ApiCall<'a> = Async<Choice<'a, Error>>

    type CardQueryFields = 
    | Name of string list

    type Card = {
        Name : string
        ImageUrl : Uri
    } with
      static member BackupImageUri = Uri "https://hydra-media.cursecdn.com/mtgsalvation.gamepedia.com/f/f8/Magic_card_back.jpg"
      static member FromJson (_ : Card) = 
        fun name img -> {Name = name; ImageUrl = img}
        <!> Json.read "name"
        <*> Json.readWithOrDefault uriFromJson "imageUrl" Card.BackupImageUri
    
    type CardResponse = {
        Cards : Card list
    } with
        static member FromJson (_ : CardResponse) = 
            fun cards -> { Cards = cards }
            <!> Json.read "cards"
    
    let joinOR strings = String.concat "|" strings
    let joinAND strings = String.concat "," strings
    
    let queryToQS = function
    | Name names -> "name", joinOR names

    let combineUri (source : Uri) (path : string) = 
        let b = UriBuilder(source)
        b.Path <- path
        b.Uri
    
    let inline handle r : ApiCall<'a> = 
        async {
            let! response = getResponse r |> Job.toAsync
            let! body = Response.readBodyAsString response |> Job.toAsync
            if response.statusCode >= 400 
            then return Json.parse body |> Json.deserialize |> Choice2Of2
            else return Json.parse body |> Json.deserialize |> Choice1Of2
        }

    let queryCards queries : ApiCall<Card list> = 
        let r = Request.create Get (combineUri rooturl "/v1/cards")
        let queryAdded = queries |> Seq.map queryToQS |> Seq.fold (fun r (k,v) -> Request.queryStringItem k v r) r
        handle queryAdded |> Async.map (Choice.bind (fun cards -> Choice1Of2 cards.Cards))
            

module MTG =   
    open System
    open Slack
    open Slack.OutboundTypes
    open mtgio

    type Command = 
        | Cards of args : Map<string, string list> 
        | Card of name : string
        | Help
    
    let (|Text|_|) (s :string) v = if s.Equals(v, StringComparison.OrdinalIgnoreCase) then Some Text else None
    let makeCardName (parts : string list) = parts |> String.concat " "

    let parseCardsArgs (args : string list) =
        args
        |> List.map (String.split '=' >> fun l -> List.item 0 l,  List.item 1 l)
        |> List.fold (fun m (k,v) ->
            match m |> Map.tryFind k with
            | None -> m |> Map.add k [v]
            | Some vs -> m |> Map.add k (v :: vs)
        ) Map.empty

    let makeErrorResponse (err : Error) : SlackResponse = 
        [ Attachment.simple (sprintf "Error: %s" err.error) ] |> SlackResponse.ofAttachments
    
    let makeCardsResponse (cards : Card list) : SlackResponse = 
        [ cards |> List.map (fun c -> c.Name) |> String.concat "\n" |> Attachment.simple ] |> SlackResponse.ofAttachments
    
    let makeCardResponse (card : Card) : SlackResponse =
        [ { Attachment.simple (card.Name) with Image = Some card.ImageUrl } ] |> SlackResponse.ofAttachments
    
    let cardArgsToGetParams (p : Map<string, string list>) : CardQueryFields list =
        let matcher l k vs = 
            match k with
            | Text "name" -> Name vs :: l
            | _ -> l

        p |> Map.fold matcher []

    let handleCard args : Async<SlackResponse> = 
        async {
            let! response = mtgio.queryCards (Seq.singleton (mtgio.Name [makeCardName args]))
            match response with
            | Choice1Of2 [] -> return makeErrorResponse { error = "could not find a card with that name" }
            | Choice1Of2 (x::_) -> return makeCardResponse x
            | Choice2Of2 err -> return makeErrorResponse err
        }
    
    let handleCards args : Async<SlackResponse> = 
        async {
            let! response = args |> parseCardsArgs |> cardArgsToGetParams |> mtgio.queryCards
            match response with
            | Choice2Of2 err -> return makeErrorResponse err 
            | Choice1Of2 cards ->  return makeCardsResponse cards
        }

    let confusedResponse command = 
        let text = sprintf "Sorry, I didn't understand the command '%s'." command
        { Attachment.simple text with 
            Title = Some <| Plain "Unknown Command" }
    let cardCommand : SlackCommand = { name = "card" 
                                       usage = "card CARDNAME" 
                                       handler = handleCard }
    let cardsCommand : SlackCommand = { name = "cards"
                                        usage = "cards FILTER=VALUE"
                                        handler = handleCards }
    let normalCommandSet = 
        let actualCommands = [
            cardCommand
            cardsCommand
        ]

        let makeHelpCommand (cmds : SlackCommand list) : SlackCommand =
            let usage = "Usage:" :: (cmds |> List.map (fun c -> c.usage)) |> String.concat "\n"
            { name = "help"
              usage = usage
              handler = fun _ -> Async.result (SlackResponse.ofAttachments [ Attachment.simple usage ]) }

        actualCommands
        |> List.fold (fun m c -> m |> Map.add c.name c) Map.empty
        |> Map.add "help" (makeHelpCommand actualCommands)