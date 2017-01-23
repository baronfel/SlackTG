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
    
    type 'a BooleanExpression = 
    | Value of inner : 'a
    | AND of exprs : ('a BooleanExpression) list
    | OR of exprs : ('a BooleanExpression) list

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

    type Color =
    | White
    | Blue
    | Black
    | Red
    | Green
    with
        static member ToFullString (c : Color) = sprintf "%A" c
        static member ToShortString = function | White -> "W" | Blue -> "U" | Black -> "B" | Red -> "R" | Green -> "G"

    type CardQueryFields = 
    | Name of string list
    | CMC of int * NumericComparison
    | Colors of Color BooleanExpression
    | Text of string BooleanExpression
    
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
    
    let joinOR = String.concat "|"
    let joinAND = String.concat ","

    let rec evalExpr (stringer : 'a -> string) expr = 
        match expr with 
        | Value s -> stringer s
        | AND xs -> xs |> List.map (evalExpr stringer) |> joinAND
        | OR xs -> xs |> List.map (evalExpr stringer) |> joinOR
    
    let queryToQS query = 
        match query with
        | Name names -> "name", joinOR names
        | CMC (value, cmp) -> "cmc", sprintf "%s%d" (NumericComparison.ToQS cmp) value
        | Colors expr -> "colors", evalExpr Color.ToFullString expr
        | Text expr -> "text", evalExpr id expr

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

[<RequireQualifiedAccess>]
module CardArgParser = 
    open mtgio
    open FParsec
    open System

    type Parser<'a> = Parser<'a, unit>

    /// given a parser `p` and a separator `sep`, `sepByMustSep p sep` parses one or more instances of `p`, as long as `sep` is matched at least once
    let sepByMustSep (p : Parser<'a,'state>) (sep : Parser<'b,'state>) : Parser<'a list,'state> = 
        let rec parseInternal hasParsedSep (current : 'a list) : CharStream<'state> -> Reply<'a list> = 
            fun (stream : CharStream<'state>) -> 
                let result = p stream
                if result.Status = Ok
                then 
                    let sepResult = sep stream
                    match sepResult.Status, hasParsedSep with
                    | Ok, _ -> 
                        // if we match the separator, parse another element
                        let resp = parseInternal true (result.Result :: current) stream
                        Reply(Ok, resp.Result, result.Error)
                    | Error, true -> 
                        // if we didn't match the separator but we have in the past we've come to the end of the list
                        Reply(Ok, result.Result :: current, result.Error)
                    | Error, false -> 
                        // if we didn't match the separator, and never have, fail the parser
                        Reply(Error, sepResult.Error)
                    | _,_ ->
                        Reply(Error, sepResult.Error)
                else
                    Reply(Error, result.Error)
                    
        parse {
            let! ps = parseInternal false []
            return List.rev ps
        }

    // booleans
    let pConjunction pterm psplitter conj = sepByMustSep pterm psplitter |>> (List.map Value >> conj)
    let pOr pterm = pConjunction pterm (pstringCI "|") OR
    let pAnd pterm = pConjunction pterm (pstringCI ",") AND

    // colors
    let parseSingleColor (short : char) (long : string) (du : Color) : Parser<Color> = skipStringCI long <|> (skipChar short <|> skipChar (Char.ToUpper short)) >>% du
    let pwhite = parseSingleColor 'w' "white" White
    let pblue = parseSingleColor 'u' "blue" Blue
    let pblack = parseSingleColor 'b' "black" Black
    let pred = parseSingleColor 'r' "red" Red
    let pgreen = parseSingleColor 'g' "green" Green
    let pColor = choice [pwhite; pblue; pblack; pred; pgreen]
    let pColorQuery = 
        skipStringCI "color=" 
        // TODO: figure out how to make the Or parser (and eventually the And parser) not super-greedy
        >>. choice [ attempt (pColor .>> (followedBy eof <|> followedBy spaces1)) |>> Value
                     attempt (pAnd pColor)
                     pOr pColor ]
        |>> Colors
    
    // CMC
    let pSkipReturn skip ret = skipStringCI skip >>% ret
    let plte = pSkipReturn "lte" LTE
    let pgte = pSkipReturn "gte" GTE
    let pgt = pSkipReturn "gt" GT
    let plt = pSkipReturn "lt" LT
    let peq = pSkipReturn "eq" EQ 
    let pNumberComparison : Parser<NumericComparison> = choice [plte; pgte; plt; pgt; peq] <|>% EQ // numeric comparisons always default to EQ
    let pCMC = skipStringCI "cmc=" >>. pNumberComparison .>>. pint32 |>> fun (cmp, num) -> CMC(num, cmp)
    
    //name
    //let pName = skipStringCI "name=" >>. p

    // general query parsing entry point
    let pArg = choice [pColorQuery; pCMC; ] 
    let pArgs = many (spaces >>. pArg)

    let pResultToChoice = function | Success (r,_,_) -> Choice1Of2 r | Failure (errS,_,_) -> Choice2Of2 errS

    let parseArgs (args : string option) = 
        args 
        |> Option.map (runParserOnString pArgs () "cardArgs")
        |> Option.map pResultToChoice
        |> Option.orDefault (Choice1Of2 [])

module MTG =   
    open System
    open Slack
    open Slack.OutboundTypes
    open mtgio
    open Text
    
    let makeErrorResponse (err : Error) = [ Attachment.simple (sprintf "Error: %s" err.error) ] |> SlackResponse.ofAttachments
    
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
        async {
            match CardArgParser.parseArgs arg with
            | Choice2Of2 errS -> return makeErrorResponse { error = errS }
            | Choice1Of2 args -> 
                let! response = mtgio.queryCards args
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