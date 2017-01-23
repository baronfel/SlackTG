module SlackTG.Tests

open SlackTG
open MTG
open Chiron
open Expecto
open HttpFs
open HttpFs.Client
open Chiron.Mapping
open Chiron.Formatting
open Hopac
open mtgio

//let cardIsColor color (card : Card) = card.Colors |> Array.exists ((=) color)

[<Tests>]
let tests = 
    testList "API" [
        testCase "can do simple query" <| fun () ->             
            let query = Some "name=Avacyn cmc=gt3 color=white color=red"
            let response = MTG.handleCards query |> Async.RunSynchronously
            Expect.equal response.ResponseType Slack.OutboundTypes.InChannel "should be in-channel response"

        testCase "can respond to pretend payload" <| fun () -> 
            let slackFormPost = [
                "token","lQ7BU6rfjWBYk2TpRaeRtJQN"
                "team_id","T0001"
                "team_domain","example"
                "channel_id","C2147483705"
                "channel_name","test"
                "user_id","U2147483697"
                "user_name","Steve"
                "command","/mtg"
                "text","cards"
                "response_url","https://hooks.slack.com/commands/1234/5678"
            ]

            let cancellationToken = new System.Threading.CancellationTokenSource()
            let started, listening = Suave.Web.startWebServerAsync Suave.Web.defaultConfig SlackTG.Suave.app
            Async.Start(listening, cancellationToken.Token)
            Async.Sleep (1000) |> Async.RunSynchronously

            let response = 
                Request.createUrl Post "http://127.0.0.1:8080/message"
                |> Request.body (
                        RequestBody.BodyForm (
                            slackFormPost |> List.map NameValue
                        )
                    )
                |> getResponse |> Job.toAsync
                |> Async.bind (Response.readBodyAsString >> Job.toAsync)
                |> Async.RunSynchronously
                
            let response : Slack.OutboundTypes.SlackResponse = response |> (Json.parse >> Json.deserialize)
            
            Expect.equal response.ResponseType SlackTG.Slack.OutboundTypes.ResponseType.InChannel "should be in-channel because not broken"
            let attachment = response.Attachments.[0]
            let (Slack.OutboundTypes.SlackText.Plain(text)) = attachment.Text.Value;
            Expect.stringStarts (text |> String.split '\n' |> List.head) "<http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=74252|_____ (UNH)>" "should start with formatted guy card."
            cancellationToken.Cancel()
            
        testCase "can write response" <| fun () -> 
            let response = Slack.OutboundTypes.SlackResponse.ofAttachments [ Slack.OutboundTypes.Attachment.simple "yeah man" ]
            let json = Json.serialize response |> Json.format
            let parsed = Json.parse json
            match parsed with
            | Object keys -> 
                Expect.isFalse (keys |> Map.containsKey "title") "shouldn't have a title"
                Expect.isFalse (keys |> Map.containsKey "image_url") "shouldn't have an image"
                let responseType : Slack.OutboundTypes.ResponseType = keys |> Map.find "response_type" |> Json.deserialize
                Expect.equal Slack.OutboundTypes.ResponseType.InChannel responseType "should be an in-channel message"
            | _ -> failwith "boom"
        testCase "rarities sort correctly" <| fun () -> 
            let mythic : Rarity = String "Mythic Rare" |> Json.deserialize
            let uncommon : Rarity = String "Uncommon" |> Json.deserialize 
            let rare : Rarity = String "Rare" |> Json.deserialize
            Expect.isGreaterThan Rarity.Mythic Rarity.Uncommon "mythic should be higher"
            Expect.isGreaterThan mythic uncommon "parsed rarities are ok"
            Expect.isGreaterThan rare uncommon "rare is higher"
            Expect.equal Rarity.Mythic mythic "mythic is the same"
            Expect.equal Rarity.Uncommon uncommon "uncommon is the same"
            Expect.equal Rarity.Rare rare "rare is the same"
    ]

open FParsec

let inline run parser s = 
    match FParsec.CharParsers.runParserOnString parser () "test" s with
    | ParserResult.Success(r, state, endPos) -> r
    | ParserResult.Failure(errS, err, state) -> failwith errS

let extractArgs (arg : string) = 
    Some arg 
    |> CardArgParser.parseArgs 
    |> function | Choice2Of2 errS -> failwith errS | Choice1Of2 args -> args

[<Tests>]
let parse = 
    testList "parsing" [
        testCase "can parse all simple colors" <| fun () -> 
            [White; Blue; Black; Red; Green]
            |> List.iter (fun color -> 
                    let input = sprintf "color=%A" color
                    let output = extractArgs input
                    let expected = [Colors(Value color)]
                    Expect.equal 1 output.Length "should only have the one arg"
                    Expect.equal output expected "should parse color" )
        
        testCase "can parse and of stuff" <| fun () -> 
            let fooParser = CardArgParser.pOr (pstringCI "foo")
            let output = run fooParser "foo|foo|foo"
            let expected = OR [Value "foo"; Value "foo"; Value "foo"]
            Expect.equal output expected "can get the foos"

        testCase "can parse or-ed colors" <| fun () -> 
            let output = run (CardArgParser.pOr CardArgParser.pColor) "U|B|W"
            let expected = OR([Value Blue; Value Black; Value White])
            Expect.equal output expected  "or-d colors should parse"

        testCase "can parse and-ed colors" <| fun () -> 
            let output = run (CardArgParser.pAnd CardArgParser.pColor) "u,b,w"
            let expected = AND([Value Blue; Value Black; Value White])
            Expect.equal output expected "and-d colors should parse"

        testCase "throws on bad query" <| fun () ->     
            Expect.throws (fun _ -> extractArgs "color=u,b|W" |> ignore) "should throw if query is malformed"

        testCase "can parse CMC" <| fun () -> 
            let parsed = extractArgs "cmc=5"
            Expect.equal [CMC(5,EQ)] parsed "should parse simple equals"

        testCase "can parse all explicit comparisons" <| fun () -> 
            [EQ; LTE; LT; GT; GTE] 
            |> List.iter (fun cmp -> 
                let result = extractArgs (sprintf "cmc=%A2" cmp)
                Expect.equal [CMC(2,cmp)] result (sprintf "comparison %A should parse" cmp))

        testCase "can parse complex, multi-condition query" <| fun () -> 
            let output = extractArgs "color=u,b cmc=lt2"
            let expected = [Colors(AND([Value Blue; Value Black])); CMC(2, LT)]
            Expect.equal output expected "should equals, yo"
        
        testCase "can do it with or, too" <| fun () -> 
            let output = extractArgs "color=u|b cmc=lt2"
            let expected = [Colors(OR [Value Blue; Value Black]); CMC(2, LT)]
            Expect.equal output expected "should equals, yo"
            
    ]

[<EntryPoint>]
let main argv = 
    runTestsInAssembly defaultConfig argv