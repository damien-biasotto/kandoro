module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html exposing (Html, button, div, footer, h1, h2, h4, header, li, nav, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Html.Styled exposing (toUnstyled)
import KandoroTask exposing (KTask, State, getDescription, getId, getState, getTimer, getTitle, newTask, setTimer)
import Styles exposing (defaultPalette, style)
import Task
import Time exposing (utc)
import Timer as T exposing (Msg, State(..), update)
import UUID
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { tasks : List KTask
    , timezone : Time.Zone
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { tasks =
            [ newTask "Implement Drag and Drop" "To allow a task to change its state and update timer" []
            , newTask "Allow a task to change its state" "Any state can be set on a task." []
            , newTask "Add persistence" "Leverage local storage as config and data storage.." []
            , newTask "Persistence part 2" "Use a backend to store stuff in database Keep the frontend storage for initial rendering?." []
            , newTask "Finish kandoro" "Use it." []
            ]
      , timezone = utc
      , key = key
      , url = url
      }
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Tick Time.Posix
    | StartTimer T.Timer
    | PauseTimer T.Timer
    | RestartTimer T.Timer
    | EndTimer T.Timer
    | AddTimestamp Time.Posix
    | AdjustTimeZone Time.Zone
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


getTasksWithRunningTimers : Model -> List KTask
getTasksWithRunningTimers model =
    List.filter (\task -> T.getState (getTimer task) == T.Running) model.tasks


getTasksWithNonRunningTimers : Model -> List KTask
getTasksWithNonRunningTimers model =
    List.filter (\task -> T.getState (getTimer task) /= T.Running) model.tasks


updateTimers : (T.Timer -> T.Msg) -> List KTask -> List KTask
updateTimers msg tasks =
    List.map (\task -> setTimer task (Tuple.first (T.update <| msg (getTimer task)))) tasks


updateTimer : (T.Timer -> T.Msg) -> T.Timer -> List KTask -> List KTask
updateTimer msg timer tasks =
    List.map
        (\task ->
            if timer == getTimer task then
                setTimer task (Tuple.first (T.update <| msg <| getTimer task))

            else
                task
        )
        tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | timezone = zone }, Cmd.none )

        Tick _ ->
            ( { model | tasks = updateTimers T.Update model.tasks }, Task.perform AddTimestamp Time.now )

        StartTimer timer ->
            ( { model | tasks = updateTimer T.Start timer model.tasks }, Task.perform AddTimestamp Time.now )

        PauseTimer timer ->
            ( { model | tasks = updateTimer T.Pause timer model.tasks }, Task.perform AddTimestamp Time.now )

        RestartTimer timer ->
            ( { model | tasks = updateTimer T.Restart timer model.tasks }, Task.perform AddTimestamp Time.now )

        EndTimer timer ->
            ( { model | tasks = updateTimer T.End timer model.tasks }, Task.perform AddTimestamp Time.now )

        AddTimestamp time ->
            ( { model | tasks = updateTimers (T.TimestampTransition time) model.tasks }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


viewBoard : Model -> Html Msg
viewBoard model =
    div [ class "board" ]
        [ nav [ class "navbar--app-bar" ]
            [ h1 [] [ text "Kandoro" ]
            ]
        , nav [ class "navbar--board-bar" ] []
        , div [ class "board--content" ]
            [ displayList KandoroTask.Todo model.tasks
            , displayList KandoroTask.Doing model.tasks
            , displayList KandoroTask.Done model.tasks
            , displayList KandoroTask.Blocked model.tasks
            ]
        ]


displayList : KandoroTask.State -> List KTask -> Html Msg
displayList state tasks =
    let
        stateAsString =
            KandoroTask.stateToString state
    in
    div [ class <| "board--column board--column__" ++ String.toLower stateAsString ]
        [ header [] [ text stateAsString ]
        , displayTasksInList <| List.filter (\task -> state == getState task) tasks
        , footer [] [ text "Add a task" ]
        ]


displayTasksInList : List KTask -> Html Msg
displayTasksInList tasks =
    Keyed.ul [] (List.map displayKeyedTaskAsListItem tasks)


displayKeyedTaskAsListItem : KTask -> ( String, Html Msg )
displayKeyedTaskAsListItem task =
    ( UUID.toString <| getId task, lazy displayTaskAsListItem task )


displayTaskAsListItem : KTask -> Html Msg
displayTaskAsListItem task =
    li []
        [ h4 [] [ text <| getTitle task ]
        , p [] [ text <| getDescription task ]
        , footer []
            [ button [ onClick (StartTimer <| getTimer task) ] [ text "Start Task" ]
            , button [ onClick (PauseTimer <| getTimer task) ] [ text "Pause task" ]
            , button [ onClick (RestartTimer <| getTimer task) ] [ text "Reset task" ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Kandoro - The Kanban Pomodoro App"
    , body =
        [ toUnstyled <| style defaultPalette
        , viewBoard model
        ]
    }


viewTask : KTask -> Html Msg
viewTask task =
    div []
        [ header []
            [ h4 [] [ text <| getTitle task ]
            ]
        , p [] [ text <| getDescription task ]
        , footer []
            [ button [ onClick (StartTimer <| getTimer task) ] [ text "Start Task" ]
            , button [ onClick (PauseTimer <| getTimer task) ] [ text "Pause task" ]
            , button [ onClick (RestartTimer <| getTimer task) ] [ text "Reset task" ]
            ]
        ]


viewTransition : T.Transition -> Html Msg
viewTransition transition =
    li [] [ text <| T.displayTransition transition ]
