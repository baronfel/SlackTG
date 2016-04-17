module SlackTG.Tests

open SlackTG
open NUnit.Framework
open Deckbrew

[<Test>]
let ``can do simple query``() =
    let query = [
        API.Color Types.Green
        API.Format Types.Commander
    ]
    let response = API.getCards query |> Async.RunSynchronously
    match response with
    | Choice2Of2 (errs) -> Assert.Fail(errs.Errors |> String.concat ";")
    | Choice1Of2 (cards) -> printf "%A" cards

[<Test>]
let ``can respond to pretend slack payload``() =
    let slackFormPost = 
        [
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
        |> List.map (fun (k,v) -> sprintf "%s=%s" k v) 
        |> String.concat "&"
        |> System.Text.Encoding.UTF8.GetBytes
        |> fun bs -> { Suave.Http.HttpRequest.empty with rawForm = bs }

    let response = 
        Suave.handleMessage slackFormPost
        |> Async.RunSynchronously
        |> string
    printf "%s" response
    Assert.That(not <| response.Contains("Error:"))