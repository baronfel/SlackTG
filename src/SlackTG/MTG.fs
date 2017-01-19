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
    open Text

    let rooturl = Uri "https://api.magicthegathering.io/v1"

    type Error = 
      { error : string } 
      with 
        static member FromJson (_ : Error) = 
            fun (e : string) -> { error = e; } 
            <!> Json.read "error"

    
    type ApiCall<'a> = Async<Choice<'a, Error>>

    type NumericComparison = | GT | GTE | LT | LTE | EQ
    with static member ToQS = function | GT -> "gt" | GTE -> "gte" | LT -> "lt" | LTE -> "lte" | EQ -> ""
         static member Parse (s : string) = 
            match s with
            | Text "gt" -> GT
            | Text "gte" -> GTE
            | Text "lt" -> LT
            | Text "lte" -> LTE
            | _ -> EQ
    
    type BooleanExpression = 
    | Value of s : string
    | AND of exprs : BooleanExpression list
    | OR of exprs : BooleanExpression list

    type CardQueryFields = 
    | Name of string list
    | CMC of int * NumericComparison
    | Colors of BooleanExpression
    | Text of BooleanExpression

    type Rarity =
    | BasicLand
    | Common
    | Uncommon
    | Rare
    | Mythic
    | Special
    with 
      static member FromJson (_ : Rarity) = 
        json {
            let! s = Json.Optic.get Json.String_
            match s with
            | Text.Text "basic land" -> return BasicLand
            | Text.Text "common" -> return Common
            | Text.Text "uncommon" -> return Uncommon
            | Text.Text "rare" -> return Rare
            | Text.Text "mythic rare" -> return Mythic
            | Text.Text "special" -> return Special
            | s ->
                return! Json.error (sprintf "expected one of: basic land, common, uncommon, rare, mythinc rare, or special. got %s" s)
        }

    type Card = {
        Name : string
        ImageUrl : Uri
        Set : string
        GathererUrl : Uri
        MultiverseId : int
        Rarity : Rarity
    } with
      static member BackupImageUri = Uri "https://hydra-media.cursecdn.com/mtgsalvation.gamepedia.com/f/f8/Magic_card_back.jpg"
      static member GathererUri (mid : int) = Uri (sprintf "http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=%d" mid)
      static member FromJson (_ : Card) = 
        fun name img setId mid rarity -> {Name = name; ImageUrl = img; Set = setId; GathererUrl = Card.GathererUri mid; MultiverseId = mid; Rarity = rarity}
        <!> Json.read "name"
        <*> Json.readWithOrDefault uriFromJson "imageUrl" Card.BackupImageUri
        <*> Json.read "set"
        <*> Json.readOrDefault "multiverseid" 0
        <*> Json.read "rarity"
    
    type CardResponse = {
        Cards : Card list
    } with
        static member FromJson (_ : CardResponse) = 
            fun cards -> { Cards = cards }
            <!> Json.read "cards"
    
    let joinOR strings = String.concat "|" strings
    let joinAND strings = String.concat "," strings

    let rec evalExpr (expr : BooleanExpression) = 
        match expr with 
        | Value s -> s
        | AND xs -> xs |> List.map evalExpr |> joinAND
        | OR xs -> xs |> List.map evalExpr |> joinOR
    
    let queryToQS = function
    | Name names -> "name", joinOR names
    | CMC (value, cmp) -> "cmc", sprintf "%s%d" (NumericComparison.ToQS cmp) value
    | Colors expr -> "colors", evalExpr expr
    | Text expr -> "text", evalExpr expr

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
        let ordered = queryAdded |> Request.queryStringItem "orderBy" "name"
        ordered |> handle |> Async.map (Choice.bind (fun cards -> Choice1Of2 cards.Cards))
    
    let makeBooster setname : ApiCall<Card list> = 
        let r = Request.create Get (combineUri rooturl (sprintf "/v1/sets/%s/booster" setname))
        r |> handle |> Async.map (Choice.bind (fun cards -> Choice1Of2 cards.Cards))

module MTG =   
    open System
    open Slack
    open Slack.OutboundTypes
    open mtgio
    open Text
    
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
    
    
    let formattedUrl (uri : Uri) (label : string) = sprintf "<%s|%s>" (string uri) label

    let fancyName (c : Card) = sprintf "%s (%s)" c.Name c.Set

    let makeCardsResponse (cards : Card list) : SlackResponse = 
        cards
        |> List.map (fun c -> formattedUrl c.GathererUrl (fancyName c)) 
        |> String.concat "\n" 
        |> Attachment.simple
        |> List.singleton 
        |> SlackResponse.ofAttachments
    

    let makeCardResponse (card : Card) : SlackResponse =
        { Attachment.simple (formattedUrl card.GathererUrl (fancyName card)) 
            with Image = Some card.ImageUrl }
        |> List.singleton
        |> SlackResponse.ofAttachments
    
    let tryParseInt (s : string) = 
        match Int32.TryParse s with
        | true, v -> Some v
        | _ -> None
    
    let cardArgsToGetParams (p : Map<string, string list>) : CardQueryFields list =
        let unwrapCollection (vs : string list) : BooleanExpression option = 
            let colors = vs |> List.collect (String.split ',') |> List.collect (String.split '|')
            match colors with
            | [] -> None 
            | [x] -> Some (Value x)
            | xs -> xs |> List.map Value |> OR |> Some

        let matcher l k vs = 
            match k with
            | Text "name" -> Name vs :: l
            | Text "cmc" ->
                match vs with
                | [] -> l
                | x::_ -> 
                    let cmp = x |> Seq.takeWhile (Char.IsDigit >> not) |> Seq.toArray |> String |> NumericComparison.Parse
                    let number = x |> Seq.skipWhile (Char.IsDigit >> not) |> Seq.takeWhile (Char.IsDigit) |> Seq.toArray |> String |> tryParseInt
                    match number with
                    | Some n -> CMC(n, cmp) :: l
                    | None -> l
            | Text "color" -> 
                match unwrapCollection vs with
                | None -> l
                | Some expr -> Colors expr :: l
            | Text "text" -> 
                match unwrapCollection vs with
                | None -> l
                | Some expr -> Text expr :: l
            | _ -> l

        p |> Map.fold matcher []

    let handleCard arg : Async<SlackResponse> = 
        let cardName = defaultArg arg ""
        async {
            let! response = mtgio.queryCards (Seq.singleton (mtgio.Name [cardName]))
            match response with
            | Choice1Of2 [] -> return makeErrorResponse { error = "could not find a card with that name" }
            | Choice1Of2 (x::_) -> return makeCardResponse x
            | Choice2Of2 err -> return makeErrorResponse err
        }
    
    let handleCards arg : Async<SlackResponse> = 
        //TODO: actual parsing lib here
        let args = defaultArg (arg |> Option.map (String.split ' ')) []
        async {
            let! response = args |> parseCardsArgs |> cardArgsToGetParams |> mtgio.queryCards
            match response with
            | Choice2Of2 err -> return makeErrorResponse err 
            | Choice1Of2 cards ->  return makeCardsResponse cards
        }
    let makeBooster arg : Async<SlackResponse> = 
        async {
            match arg with
            | None -> return makeErrorResponse { error = "gotta specify a set, dude" }
            | Some setname -> 
                let! cards = mtgio.makeBooster setname
                match cards with
                | Choice2Of2 err -> return makeErrorResponse err
                | Choice1Of2 cards -> return makeCardsResponse (cards |> List.sortByDescending (fun c -> c.Rarity))
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
    let boosterCommand : SlackCommand = { name = "booster"
                                          usage = "booster SETNAME" 
                                          handler = makeBooster }
    let normalCommandSet = 
        let actualCommands = [
            cardCommand
            cardsCommand
            boosterCommand
        ]

        let makeHelpCommand (cmds : SlackCommand list) : SlackCommand =
            let usage = "Usage:" :: (cmds |> List.map (fun c -> c.usage)) |> String.concat "\n"
            { name = "help"
              usage = usage
              handler = fun _ -> Async.result (SlackResponse.ofAttachments [ Attachment.simple usage ]) }

        actualCommands
        |> List.fold (fun m c -> m |> Map.add c.name c) Map.empty
        |> Map.add "help" (makeHelpCommand actualCommands)