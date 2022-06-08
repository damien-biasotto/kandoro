module KandoroTask exposing (Comment, KTask, State(..), Tag, Transition(..), getDescription, getTimer, setTimer, getTitle, newTask, stateToString, getId, getState, setState, getTransitions)

import Time exposing (Posix)
import Timer exposing (Timer, newTimer)
import UUID exposing (UUID, forName, dnsNamespace)

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

appNamespace : UUID
appNamespace =
    forName "https://kandoro.github.io" dnsNamespace

widgetNamespace : UUID
widgetNamespace =
    forName "Kandoro" appNamespace

type KTask
    = Task
        { id : UUID
        , title : String
        , description : String
        , tags : List Tag
        , state : State
        , timer : Timer
        , transitions : List Transition
        , comments : List Comment
        , order : Int
        }


newTask : String -> String -> List Tag -> KTask
newTask title content tags =
    Task
        { id = forName title widgetNamespace
        , title = title
        , state = Todo
        , description = content
        , tags = tags
        , timer = newTimer (Timer.Duration 25) (Timer.Duration 15) (Timer.Duration 5)
        , transitions = []
        , comments = []
        , order = 0
        }

getId : KTask -> UUID
getId (Task task) =
    task.id

getTitle : KTask -> String
getTitle (Task task) =
    task.title


getDescription : KTask -> String
getDescription (Task task) =
    task.description


getTimer : KTask -> Timer
getTimer (Task task) =
    task.timer

getState : KTask -> State
getState (Task task) =
    task.state

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

setTimer : KTask -> Timer -> KTask
setTimer (Task task) timer =
    (Task {task| timer = timer})

setState : State -> KTask -> KTask
setState state (Task task) =
    Task {task | state = state }


getTransitions : KTask -> List Transition
getTransitions (Task task) =
    task.transitions
