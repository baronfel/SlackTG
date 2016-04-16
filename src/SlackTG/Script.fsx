// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load @"Scripts/load-references-debug.fsx"
#load @"Scripts/load-project-debug.fsx"

open SlackTG

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

Suave.handleMessage slackFormPost
|> Async.RunSynchronously
|> string
|> printf "%s"

