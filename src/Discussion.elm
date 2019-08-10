module Discussion exposing(User, Message)
import Time

type alias User = {
      username: String
    }

type alias Message = {
      user : User,
      timestamp : Time.Posix,
      topic : String,
      body : String
  }

