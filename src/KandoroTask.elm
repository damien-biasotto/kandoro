module KandoroTask exposing (Comment, KTask, State(..), Tag, Transition, getDescription, getTimer, getTitle, newTask, stateToString)

import Time exposing (Posix)
import Timer exposing (Timer, newTimer)


type State
    = Todo
    | Doing
    | Done
    | Blocked


type Transition
    = StateWithoutTime ( State, State )
    | StateWithTime ( State, State, Posix )
    | OrderWithoutTime ( Order, Order )
    | OrderWithTime ( Order, Order, Posix )


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


type User
    = User String


type KTask
    = Task
        { title : String
        , description : String
        , tags : List Tag
        , timer : Timer
        , transitions : List Transition
        , comments : List Comment
        , order : Int
        }


newTask : String -> String -> List Tag -> KTask
newTask title content tags =
    Task
        { title = title
        , description = content
        , tags = tags
        , timer = newTimer (Timer.Duration 25) (Timer.Duration 15) (Timer.Duration 5)
        , transitions = []
        , comments = []
        , order = 0
        }


getTitle : KTask -> String
getTitle (Task task) =
    task.title


getDescription : KTask -> String
getDescription (Task task) =
    task.description


getTimer : KTask -> Timer
getTimer (Task task) =
    task.timer


stateToString : State -> String
stateToString state =
    case state of
        Todo ->
            "Todo"

        Doing ->
            "Doing"

        Done ->
            "Done"

        Blocked ->
            "Blocked"
