namespace SlackTG

/// a lightweight wrapper around the api for Deckbrew <https://deckbrew.com/api/>, a MtG card api 
module Deckbrew =
    module Types = 
        open FSharp.Data
        let getJsonStrings file = 
            file 
            |> System.IO.File.ReadAllText
            |> JsonValue.Parse
            |> (fun v -> v.AsArray())
            |> Array.map (fun v -> v.AsString())

        type CardModel = JsonProvider< @"../../samples/cards.json", SampleIsList = true, RootName = "card", InferTypesFromValues = true>
        type SetsModel = JsonProvider< @"../../samples/sets.json", SampleIsList = true, RootName = "set", InferTypesFromValues = true>
        
        let CardTypes = getJsonStrings "samples/types.json"
        let SubTypes = getJsonStrings "samples/subtypes.json"
        let SuperTypes = getJsonStrings "samples/supertypes.json"

        type Rarity = Common | Uncommon | Rare | Mythic
        with override x.ToString() = match x with | Common -> "common" | Uncommon -> "uncommon" | Rare -> "rare" | Mythic -> "mythic"
        
        type Color = White | Blue | Black | Red | Green
        with override x.ToString() = match x with | White -> "white" | Blue -> "blue" | Black -> "black" | Red -> "red" | Green -> "green"
         
        type Format = Vintage | Legacy | Modern | Standard | Commander
        with override x.ToString() = match x with | Vintage -> "vintage" | Legacy -> "legacy" | Modern -> "modern" | Standard -> "standard" | Commander -> "commander"
        
        type Status = Legal | Banned | Restricted
        with override x.ToString() = match x with | Legal -> "legal" | Banned -> "banned" | Restricted -> "restricted"

        type CardType = CardType of string
        with 
            static member create (t : string) = 
                if CardTypes |> Array.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
                then CardType <| t.ToLower() |> Some
                else None
        
        type SubType = SubType of string
        with
            static member create (t : string) =
                if SubTypes |> Array.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
                then SubType <| t.ToLower() |> Some
                else None

        type SuperType = SuperType of string
        with
            static member create (t : string) = 
                if SuperTypes |> Array.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
                then SuperType <| t.ToLower() |> Some
                else None

        /// the model returned by Deckbrew for any 400+ status code
        type Error = {
            Errors : string list
        }

        type Link = Next of System.Uri | Prev of System.Uri
        type Linked<'a> = { Payload : 'a; Next : System.Uri option; Prev : System.Uri option }

    module API = 
        open HttpClient
        open FSharp.Text.RegexProvider

        let root = "https://api.deckbrew.com/mtg"

        type getRequestParameter = 
            | Type of Types.CardType
            | SubType of Types.SubType
            | SuperType of Types.SuperType
            | Name of string
            | Oracle of string
            | Set of string
            | Rarity of Types.Rarity
            | Color of Types.Color
            | Multicolor of bool
            | MultiverseId of string
            | Format of Types.Format
            | Status of Types.Status

        let getQSName (getRequestParam : getRequestParameter) = 
            match getRequestParam with
            | Type (_) -> "type"
            | SubType(_) -> "subtype"
            | SuperType(_) -> "supertype"
            | Name(_) -> "name"
            | Oracle(_) -> "oracle"
            | Set(_) -> "set"
            | Rarity(_) -> "rarity"
            | Color(_) -> "color"
            | Multicolor(_) -> "multicolor"
            | MultiverseId(_) -> "multiverseid"
            | Format(_) -> "format"
            | Status(_) -> "status"

        let getQSValue (getRequestParam : getRequestParameter) =
            match getRequestParam with
            | Type(Types.CardType t) -> t
            | SubType(Types.SubType st) -> st
            | SuperType(Types.SuperType st) -> st
            | Name(name) -> name
            | Oracle(oracleText) -> oracleText
            | Set(setId) -> setId
            | Rarity r -> string r
            | Color c -> string c
            | Multicolor m -> string m
            | MultiverseId mid -> mid
            | Format f -> string f
            | Status s -> string s 

        let parseErrorResponse resp =
            match resp with
            | None -> { Types.Error.Errors = [] }
            | Some r -> 
                let errs = r |> FSharp.Data.JsonValue.ParseMultiple |> Seq.map FSharp.Data.JsonExtensions.AsString |> List.ofSeq
                { Errors = errs }
        
        let uriLink link = 
            match link with
            | Types.Next u -> u
            | Types.Prev u -> u

        type LinkProvider = Regex<"<(?<uri>[\S]*)>;\srel=\"(?<rel>next|prev)\"">

        let parseLinks headers =
            let createLink s =
                let m = LinkProvider().Match(s)
                if m.Success 
                then 
                    match m.rel.Value with
                    | "next" -> Types.Next (System.Uri(m.uri.Value)) |> Some
                    | "prev" -> Types.Prev (System.Uri(m.uri.Value)) |> Some
                    | _ -> None
                else None

            match headers |> Map.tryFind ResponseHeader.Link with
            | None -> None, None
            | Some (link : string) -> 
                let potentialLinks = link.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim()) |> Array.map createLink |> Array.choose id
                let prevs, nexts = potentialLinks |> Array.partition (fun l -> match l with Types.Prev _ -> true | _ -> false)
                Array.tryHead prevs, Array.tryHead nexts
        
        let doRequest r successF = 
            async {
                let! response = getResponseAsync r
                if response.StatusCode >= 400
                then return response.EntityBody |> parseErrorResponse |> Choice2Of2
                else return successF response |> Choice1Of2
            }

        let getCards : #seq<getRequestParameter> -> Async<Choice<Types.Linked<Types.CardModel.Card list>, Types.Error>> =
            let qsParamToNameValue qsp = 
                { name = getQSName qsp
                  value = getQSValue qsp }

            let addQueryString (parameters : #seq<getRequestParameter>) request = 
                parameters 
                |> Seq.map qsParamToNameValue
                |> Seq.fold (fun request p -> withQueryStringItem p request) request

            let parseCards res =
                let parseCards cardsResponse = 
                    match cardsResponse with
                    | None -> []
                    | Some cs -> 
                        let multiples = cs |> FSharp.Data.JsonValue.Parse |> FSharp.Data.JsonExtensions.AsArray |> Array.toList
                        let stringified = multiples |> List.map string 
                        let cards = stringified |> List.map (Types.CardModel.Parse)
                        cards

                let prev, next = parseLinks res.Headers
                let cards = parseCards res.EntityBody
                { Types.Linked.Payload = cards; Types.Linked.Next = Option.map uriLink next; Types.Linked.Prev = Option.map uriLink prev; }

            fun p ->
                let req = 
                    createRequest Get (sprintf "%s/cards" root)
                    |> addQueryString p
                doRequest req parseCards
        
        let getCard : string -> Async<Choice<Types.CardModel.Card, Types.Error>> =
            fun id -> 
                let req = 
                    createRequest Get (sprintf "%s/cards/%s" root id)
                doRequest req (fun r -> r.EntityBody.Value |> Types.CardModel.Parse)