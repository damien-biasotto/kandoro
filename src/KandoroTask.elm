module KandoroTask exposing ()

import Timer exposing (Timer, newTimer)
import Time exposing (Posix)

type State
    = Todo
    | Doing
    | Done
    | Blocked

type Transition
    = StateWithoutTime ( State, State )
    | StateWithTime ( State, State, Posix )
    | OrderWithoutTime (Order, Order)
    | OrderWithTime (Order, Order, Posix)

type Tag
    = Tag String

type Comment
    = CommentWithoutDate
        { author : User
        , comment : String
        }
    | CommentWithDate
        { author : User
        , comment : String
        , createdAt : Posix
        }

type Order
    = Order State Int

type Task
    = Task
    { title : String
    , description : String
    , tags : List Tag
    , timer : Timer
    , transitions : List Transition
    , comments : List Comment
    , order
    }

newTask : String -> String -> List Tag -> Task
newTask title content tags
    = Task
        { title = title
        , description = content
        , tags = tags
        , timer = newTimer (Duration 25) (Duration 15) (Duration 5)
        , transitions = []
        , comments = []
        }
