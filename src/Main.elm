module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html exposing (Html, button, div, footer, h1, h4, header, li, nav, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed exposing (ul)
import Html.Lazy exposing (lazy)
import Html.Styled exposing (toUnstyled)
import Html5.DragDrop as DragDrop
import KandoroTask as K exposing (KTask, State, getDescription, getId, getState, getTimer, getTitle, newTask, setState, setTimer)
import Styles exposing (defaultPalette, style)
import Task exposing (Task)
import Time exposing (utc)
import Timer as T exposing (Msg, State(..), update)
import UUID exposing (UUID)
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
    , dragDrop : DragDrop.Model UUID K.State
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { tasks =
            [ setState K.Done <| newTask "Implement Drag and Drop" "To allow a task to change its state and update timer" []
            , newTask "Add UI to CRUD tasks" "Welp, at some point we should be able to add/edit/remove tasks" []
            , newTask "Audio feedback when transitioning" "So we know in which cycle we are (focus/ break/ longbreak)" []
            , newTask "Allow a task in progress to be paused / resumed." "" []
            , newTask "Restrict the number of in progress tasks" "Pick the number from the config (or use default one)" []
            , newTask "Add persistence" "Leverage local storage as config and data storage.." []
            , newTask "Persistence part 2" "Use a backend to store stuff in database Keep the frontend storage for initial rendering?." []
            , newTask "Show stats about tasks" "get stats for a given task and its timer" []
            , newTask "Finish kandoro" "Use it." []
            ]
      , timezone = utc
      , key = key
      , url = url
      , dragDrop = DragDrop.init
      }
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Ticked Time.Posix
    | StartedTimer KTask
    | PausedTimer KTask
    | RestartedTimer KTask
    | EndedTimer KTask
    | AddTimestamp KTask Time.Posix
    | AdjustTimeZone Time.Zone
    | TaskStateChanged KTask K.State
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | DragDropMsg (DragDrop.Msg UUID K.State)


getTasksWithRunningTimers : Model -> List KTask
getTasksWithRunningTimers model =
    List.filter (\task -> T.getState (getTimer task) == T.Running) model.tasks


getTasksWithNonRunningTimers : Model -> List KTask
getTasksWithNonRunningTimers model =
    List.filter (\task -> T.getState (getTimer task) /= T.Running) model.tasks


updateTimers : (T.Timer -> T.Msg) -> List KTask -> List KTask
updateTimers msg tasks =
    List.map (\task -> setTimer task (Tuple.first (T.update <| msg (getTimer task)))) tasks


updateTimer : (T.Timer -> T.Msg) -> KTask -> List KTask -> List KTask
updateTimer msg task tasks =
    List.map
        (\t ->
            if getId t == getId task then
                setTimer task (Tuple.first (T.update <| msg <| getTimer t))

            else
                t
        )
        tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | timezone = zone }, Cmd.none )

        Ticked _ ->
            ( { model | tasks = updateTimers T.Update model.tasks }, Cmd.batch <| List.map (\task -> Task.perform (AddTimestamp task) Time.now) model.tasks )

        StartedTimer task ->
            ( { model | tasks = updateTimer T.Start task model.tasks }, Task.perform (AddTimestamp task) Time.now )

        PausedTimer task ->
            ( { model | tasks = updateTimer T.Pause task model.tasks }, Task.perform (AddTimestamp task) Time.now )

        RestartedTimer task ->
            ( { model | tasks = updateTimer T.Restart task model.tasks }, Task.perform (AddTimestamp task) Time.now )

        EndedTimer task ->
            ( { model | tasks = updateTimer T.End task model.tasks }, Task.perform (AddTimestamp task) Time.now )

        AddTimestamp task time ->
            ( { model | tasks = updateTimer (T.TimestampTransition time) task model.tasks }, Cmd.none )

        TaskStateChanged task state ->
            ( model
            , case state of
                K.Todo ->
                    Task.perform RestartedTimer (Task.succeed task)

                K.Doing ->
                    Task.perform StartedTimer (Task.succeed task)

                K.Done ->
                    Task.perform EndedTimer (Task.succeed task)

                K.Blocked ->
                    Task.perform PausedTimer (Task.succeed task)
            )

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

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            case result of
                Nothing ->
                    ( { model | dragDrop = model_ }, Cmd.none )

                Just ( taskId, state, _ ) ->
                    let
                        taskMaybe =
                            List.head <| List.filter (\t -> taskId == getId t) model.tasks
                    in
                    case taskMaybe of
                        Nothing ->
                            ( { model | dragDrop = model_ }, Cmd.none )

                        Just task ->
                            let
                                updatedTask =
                                    K.setState state task
                            in
                            ( { model
                                | dragDrop = model_
                                , tasks =
                                    List.map
                                        (\t ->
                                            if taskId == getId t then
                                                updatedTask

                                            else
                                                t
                                        )
                                        model.tasks
                              }
                            , Task.perform (TaskStateChanged updatedTask) (Task.succeed state)
                            )


getTaskWithRunningTimer : Model -> Maybe KTask
getTaskWithRunningTimer model =
    List.head <|
        List.filter
            (\task ->
                case T.getState (getTimer task) of
                    T.Running ->
                        True

                    T.ShortBreak ->
                        True

                    T.LongBreak ->
                        True

                    _ ->
                        False
            )
            model.tasks


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Ticked


viewBoard : Model -> Html Msg
viewBoard model =
    div [ class "board" ]
        [ nav [ class "navbar--app-bar" ]
            [ h1 [] [ text "Kandoro" ]
            ]
        , nav [ class "navbar--board-bar" ] [ displayTimer <| getTaskWithRunningTimer model ]
        , div [ class "board--content" ]
            [ displayList K.Todo model.tasks
            , displayList K.Doing model.tasks
            , displayList K.Done model.tasks
            , displayList K.Blocked model.tasks
            ]
        ]


displayTimer : Maybe KTask -> Html Msg
displayTimer task =
    case task of
        Just t ->
            p [] [ text <| " Task " ++ getTitle t ++ " is in progress. - " ++ T.toString (getTimer t) ]

        Nothing ->
            p [] []


displayList : K.State -> List KTask -> Html Msg
displayList state tasks =
    let
        stateAsString =
            K.stateToString state
    in
    div
        ([ class <| "board--column board--column__" ++ String.toLower stateAsString
         ]
            ++ DragDrop.droppable DragDropMsg state
        )
        [ header [] [ text stateAsString ]
        , displayTasksInList state (List.filter (\task -> state == getState task) tasks)
        , footer [] [ text "Add a task" ]
        ]


displayTasksInList : K.State -> List KTask -> Html Msg
displayTasksInList state tasks =
    ul [] (List.map displayKeyedTaskAsListItem tasks)


displayKeyedTaskAsListItem : KTask -> ( String, Html Msg )
displayKeyedTaskAsListItem task =
    ( UUID.toString <| getId task, lazy displayTaskAsListItem task )


displayTaskAsListItem : KTask -> Html Msg
displayTaskAsListItem task =
    li (DragDrop.draggable DragDropMsg (getId task))
        [ h4 [] [ text <| getTitle task ]
        , p [] [ text <| getDescription task ]
        , footer []
            [ button [ onClick (StartedTimer task) ] [ text "Start Task" ]
            , button [ onClick (PausedTimer task) ] [ text "Pause task" ]
            , button [ onClick (RestartedTimer task) ] [ text "Reset task" ]
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
            [ button [ onClick (StartedTimer task) ] [ text "Start Task" ]
            , button [ onClick (PausedTimer task) ] [ text "Pause task" ]
            , button [ onClick (RestartedTimer task) ] [ text "Reset task" ]
            ]
        ]


viewTransition : T.Transition -> Html Msg
viewTransition transition =
    li [] [ text <| T.displayTransition transition ]
