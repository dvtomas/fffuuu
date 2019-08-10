module Discussion exposing(User, Message)
import Time

type alias User = String

type alias Message = {
      username : String,
      timestamp : Time.Posix,
      topic : String,
      message : String
  }

