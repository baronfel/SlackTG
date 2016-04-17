namespace SlackTG

module Async =  
    open System.Threading.Tasks

    type AsyncBuilder with
        member x.Bind (t: Task<_>, f : 'a -> Async<_>) = async.Bind(Async.AwaitTask t, f)

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
        
        //let CardTypes = getJsonStrings "samples/types.json"
        //let SubTypes = getJsonStrings "samples/subtypes.json"
        //let SuperTypes = getJsonStrings "samples/supertypes.json"

        type Rarity = Common | Uncommon | Rare | Mythic
        with override x.ToString() = match x with | Common -> "common" | Uncommon -> "uncommon" | Rare -> "rare" | Mythic -> "mythic"
        
        type Color = White | Blue | Black | Red | Green
        with override x.ToString() = match x with | White -> "white" | Blue -> "blue" | Black -> "black" | Red -> "red" | Green -> "green"
         
        type Format = Vintage | Legacy | Modern | Standard | Commander
        with override x.ToString() = match x with | Vintage -> "vintage" | Legacy -> "legacy" | Modern -> "modern" | Standard -> "standard" | Commander -> "commander"
        
        type Status = Legal | Banned | Restricted
        with override x.ToString() = match x with | Legal -> "legal" | Banned -> "banned" | Restricted -> "restricted"

        type CardType = CardType of string
//        with 
//            static member create (t : string) = 
//                if CardTypes |> Array.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
//                then CardType <| t.ToLower() |> Some
//                else None
        
        type SubType = SubType of string
//        with
//            static member create (t : string) =
//                if SubTypes |> Array.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
//                then SubType <| t.ToLower() |> Some
//                else None

        type SuperType = SuperType of string
//        with
//            static member create (t : string) = 
//                if SuperTypes |> Array.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
//                then SuperType <| t.ToLower() |> Some
//                else None

        /// the model returned by Deckbrew for any 400+ status code
        type Error = {
            Errors : string list
        }

        type Link = Next of System.Uri | Prev of System.Uri
        type Linked<'a> = { Payload : 'a; Next : System.Uri option; Prev : System.Uri option }

    module API = 
        open FSharp.Text.RegexProvider
        open System
        open System.Net.Http
        open Async
        open System.Net.Http.Headers

        let root = Uri("https://api.deckbrew.com",UriKind.Absolute)

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
            let errs = resp |> FSharp.Data.JsonValue.ParseMultiple |> Seq.map FSharp.Data.JsonExtensions.AsString |> List.ofSeq
            { Types.Error.Errors = errs }
        
        let uriLink link = 
            match link with
            | Types.Next u -> u
            | Types.Prev u -> u

        type LinkProvider = Regex<"<(?<uri>[\S]*)>;\srel=\"(?<rel>next|prev)\"">

        let parseLinks (headers : HttpResponseHeaders) =
            let createLink s =
                let m = LinkProvider().Match(s)
                if m.Success 
                then 
                    match m.rel.Value with
                    | "next" -> Types.Next (System.Uri(m.uri.Value)) |> Some
                    | "prev" -> Types.Prev (System.Uri(m.uri.Value)) |> Some
                    | _ -> None
                else None

            match headers.GetValues("Link") |> Seq.toList with
            | [] -> None, None
            | links -> 
                let potentialLinks = links |> List.map (fun s -> s.Trim()) |> List.choose createLink
                let prevs, nexts = potentialLinks |> List.partition (fun l -> match l with Types.Prev _ -> true | _ -> false)
                List.tryHead prevs, List.tryHead nexts
        
        let doRequest (uri : Uri) successF = 
            async {
                use client = new HttpClient()
                let! response = client.GetAsync(uri)
                let! content = response.Content.ReadAsStringAsync()

                if not response.IsSuccessStatusCode
                then return content |> parseErrorResponse |> Choice2Of2
                else return successF response.Headers content |> Choice1Of2
            }

        let getCards : getRequestParameter list -> Async<Choice<Types.Linked<Types.CardModel.Card list>, Types.Error>> =
            let qsParamToNameValue qsp = getQSName qsp, getQSValue qsp

            let parseCards (headers : HttpResponseHeaders) (content : string) =
                let parseCards cardsResponse = 
                    match cardsResponse with
                    | "" -> []
                    | cs -> 
                        let multiples = cs |> FSharp.Data.JsonValue.Parse |> FSharp.Data.JsonExtensions.AsArray |> Array.toList
                        let stringified = multiples |> List.map string 
                        let cards = stringified |> List.map (Types.CardModel.Parse)
                        cards

                let prev, next = parseLinks headers
                let cards = parseCards content
                { Types.Linked.Payload = cards; Types.Linked.Next = Option.map uriLink next; Types.Linked.Prev = Option.map uriLink prev; }

            let cardsUri = Uri(root, "/mtg/cards")

            fun ps ->
                let initialQS = cardsUri.ParseQueryString() // get empty collection
                let qs = ps |> List.map qsParamToNameValue |> List.iter (fun (name, value) -> initialQS.Add(name, value);) |> string
                let newUri = UriBuilder(cardsUri)
                newUri.Query <- qs
                doRequest newUri.Uri parseCards
        
        let getCard : string -> Async<Choice<Types.CardModel.Card, Types.Error>> =
            fun id -> 
                let uri = Uri(root, sprintf "/mtg/cards/%s" id)
                doRequest uri (fun headers content -> Types.CardModel.Parse content)