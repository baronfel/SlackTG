namespace SlackTG

module Serialization = 
    open System
    open Chiron

    let uriToJson (u : Uri) = String (string u)
        
    let (|ValidUri|InvalidUri|) s = 
        try 
            let u = System.Uri(s, System.UriKind.Absolute)
            ValidUri u
        with
        | _ -> InvalidUri
        
    let uriFromJsonOpt = function
        | String (ValidUri uri) -> Value (Some uri)
        | json -> 
            Json.formatWith JsonFormattingOptions.SingleLine json
            |> sprintf "Expected a string containing an absolute URI: %s"
            |> Error
    
    let uriFromJson = function
        | String (ValidUri uri) -> Value uri
        | json -> 
            Json.formatWith JsonFormattingOptions.SingleLine json
            |> sprintf "Expected a string containing an absolute URI: %s"
            |> Error

module Slack =
    module OutboundTypes =
        open Serialization
        open Chiron
        open Chiron.Operators

        type SlackText = | Plain of string | Markdown of string
        with member x.Text = match x with Plain s -> s | Markdown m -> m
             static member ToJson (s : SlackText) = 
                match s with
                | Plain s -> Json.Optic.set Json.String_ s
                | Markdown s -> Json.Optic.set Json.String_ s
             static member FromJson (_ : SlackText) = 
                Plain <!> Json.Optic.get Json.String_
                
             static member IsMarkdownString = function | Plain _ -> false | Markdown _ -> true


        type Attachment = {
            Title : SlackText option
            PreText : SlackText option
            Text : SlackText option
            Fallback : string
            Image : System.Uri option
        }
        with
            static member ToJson (a : Attachment) = 
                 let markdownFields = 
                    seq { if defaultArg (Option.map SlackText.IsMarkdownString a.Title) false then yield "title"
                          if defaultArg (Option.map SlackText.IsMarkdownString a.PreText) false then yield "pretext"
                          if defaultArg (Option.map SlackText.IsMarkdownString a.Text) false then yield "text" } |> Seq.toArray
                 Json.write "fallback" a.Fallback
                 *> Json.writeUnlessDefault "title" None a.Title
                 *> Json.writeUnlessDefault "pretext" None a.PreText
                 *> Json.writeUnlessDefault "text" None a.Text
                 *> json {
                        match a.Image with
                        | Some uri ->
                            do! Json.write "image_url" (string uri) 
                        | None -> ()
                    }
                 *> Json.writeUnlessDefault "mrkdwn_in" [||] markdownFields
            static member FromJson (_ : Attachment) = 
                fun title pre text fallback image -> {Title = title; PreText = pre; Text = text; Fallback = fallback; Image = image;}
                <!> Json.tryRead "title"
                <*> Json.tryRead "pretext"
                <*> Json.tryRead "text"
                <*> Json.read "fallback"
                <*> Json.tryReadWith uriFromJsonOpt "image_url"
            static member simple text = {Title = None; PreText = None; Text = Some <| Plain text; Fallback = text; Image = None} 
            
        type ResponseType = | InChannel | Ephemeral
        with static member ToJson r = 
                match r with 
                | InChannel -> Json.Optic.set Json.String_ "in_channel"
                | Ephemeral -> Json.Optic.set Json.String_ "ephemeral"
             static member FromJson (_ : ResponseType) =
                json {
                    let! s = Json.Optic.get Json.String_
                    if s = "in_channel" then return InChannel
                    else if s = "ephemeral" then return Ephemeral
                    else return failwith "unknown response type"
                } 

        type SlackResponse = {
            Attachments : Attachment list
            ResponseType : ResponseType
        }
        with static member ofAttachments a = {ResponseType = InChannel; Attachments = a }
             static member ToJson (r : SlackResponse) = 
                Json.write "response_type" r.ResponseType 
                *> Json.write "attachments" r.Attachments
             static member FromJson (_ : SlackResponse) = 
                fun responseT attachs -> {Attachments = attachs; ResponseType = responseT }
                <!> Json.read "response_type"
                <*> Json.read "attachments"
        
    module InboundTypes = 
        type TeamInfo = {
            id : string
            domain : string
        }
        type ChannelInfo = {
            id : string
            name : string
        }

        type UserInfo = {
            id : string
            name : string
        }
        
        type SlackArgs = {
            token : string
            team : TeamInfo
            channel : ChannelInfo
            user : UserInfo
            command : string
            args : string list
            response_url : System.Uri
        }

    type SlackCommand = {
        name : string
        usage : string
        handler : string list -> Async<OutboundTypes.SlackResponse>
    }
                