module Message exposing(User, Model, view)
import Html exposing (..)
import Time

type alias User = {
      username: String
    }


type alias Model = {
      user : User,
      timestamp : Time.Posix,
      subject : String,
      body : String
  }

formatTime: Time.Posix -> String
formatTime time =   String.fromInt (Time.toHour Time.utc time)
                    ++ ":" ++
                    String.fromInt (Time.toMinute Time.utc time)
                    ++ ":" ++
                    String.fromInt (Time.toSecond Time.utc time)
                    ++ " (UTC)"

view model =
  div [] [
    b [] [text (model.user.username)],
    text " ",
    i [] [text (formatTime model.timestamp)],
    text " ",
    b [] [text model.subject],
    p [] [text model.body]
  ]
