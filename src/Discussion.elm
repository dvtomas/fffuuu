module Discussion exposing (Message, MessagesQuery, Msg(..), User, fetchMessagesCmd, httpErrorToString, postMessageCmd)

import Http
import Json.Decode as D
import Json.Encode as E
import Time


type alias User =
    String


type alias Message =
    { username : String
    , timestamp : Time.Posix
    , topic : String
    , message : String
    }


type alias Discussion =
    List Message


type Msg
    = MessagePosted (Result Http.Error Message)
    | MessagesList (Result Http.Error Discussion)



-- RestDB communication


apiKeyHeader =
    Http.header "x-apikey" "5d4edcc758a35b31adeba6a8"


messageApiUrl =
    "https://fffuuu-c42f.restdb.io/rest/messages"


encodeMessage : Message -> E.Value
encodeMessage message =
    E.object
        [ ( "username", E.string message.username )
        , ( "timestamp", E.int <| Time.posixToMillis message.timestamp )
        , ( "topic", E.string message.topic )
        , ( "message", E.string message.message )
        ]


decoderMessage : D.Decoder Message
decoderMessage =
    D.map4 Message
        (D.field "username" D.string)
        (D.field "timestamp" (D.map Time.millisToPosix D.int))
        (D.field "topic" D.string)
        (D.field "message" D.string)


postMessageCmd message =
    Http.request
        { method = "POST"
        , headers = [ apiKeyHeader ]
        , url = messageApiUrl
        , body = Http.jsonBody (encodeMessage message)
        , expect = Http.expectJson MessagePosted decoderMessage
        , timeout = Nothing
        , tracker = Nothing
        }


type alias MessagesQuery =
    {}


fetchMessagesCmd query =
    let
        url =
            messageApiUrl ++ "?q={}&h={\"$orderby\": {\"timestamp\": -1}, \"$max\": 200}"
    in
    Http.request
        { method = "GET"
        , headers = [ apiKeyHeader ]
        , url = url
        , body = Http.jsonBody (E.object [])
        , expect = Http.expectJson MessagesList (D.list decoderMessage)
        , timeout = Nothing
        , tracker = Nothing
        }


httpErrorToString error =
    case error of
        Http.BadUrl string ->
            "Bad URL " ++ string

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status" ++ String.fromInt status

        Http.BadBody body ->
            "Bad body" ++ body
