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
            let response = MTG.handleCards ["name=Avacyn"; "cmc=gt3"; "color=white"; "color=red"] |> Async.RunSynchronously
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
            Expect.stringStarts (text |> String.split '\n' |> List.head) "Air Elemental" "should start with Air Elemental card."
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
        ftestCase "can parse card args" <| fun () ->
            let args = ["color=blue"; "color=black"; "cmc=lt2"]
            let parsed = parseCardsArgs args
            let expected = Map.empty |> Map.add "color" ["black"; "blue";] |> Map.add "cmc" ["lt2"]
            Expect.equal parsed expected "should be able to parse sorta-complex args"
    ]

[<EntryPoint>]
let main argv = 
    runTestsInAssembly defaultConfig argv