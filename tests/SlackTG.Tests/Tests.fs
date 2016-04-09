module SlackTG.Tests

open SlackTG
open NUnit.Framework
open Deckbrew

[<Test>]
let ``can do simple query``() =
    let query = [|
        API.Color Types.Green
        API.Format Types.Commander
    |]
    let response = API.getCards query |> Async.RunSynchronously
    match response with
    | Choice2Of2 (errs) -> Assert.Fail(errs.Errors |> String.concat ";")
    | Choice1Of2 (cards) -> printf "%A" cards