module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html exposing (Html, button, div, footer, h1, h2, h4, header, li, nav, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Styled exposing (toUnstyled)
import KandoroTask exposing (KTask, State, getDescription, getTimer, getTitle, newTask)
import Styles exposing (defaultPalette, style)
import Task
import Time exposing (utc)
import Timer as T exposing (Msg, update)
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
    { timer : T.Timer
    , timezone : Time.Zone
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { timer = T.newTimer (T.Duration 2) (T.Duration 2) (T.Duration 1), timezone = utc, key = key, url = url }
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | timezone = zone }, Cmd.none )

        Tick _ ->
            ( { model | timer = Tuple.first (T.update <| T.Update model.timer) }, Task.perform AddTimestamp Time.now )

        StartTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.Start timer) }, Task.perform AddTimestamp Time.now )

        PauseTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.Pause timer) }, Task.perform AddTimestamp Time.now )

        RestartTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.Restart timer) }, Task.perform AddTimestamp Time.now )

        EndTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.End timer) }, Task.perform AddTimestamp Time.now )

        AddTimestamp time ->
            ( { model | timer = Tuple.first (T.update <| T.TimestampTransition model.timer time) }, Cmd.none )

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
            [ displayList KandoroTask.Todo
                [ newTask "Finish kandoro" "Ideally there should ne some persistence aka backend, drag and drop and a single item in Doing allowed." []
                , newTask "Finish kandoro" "Ideally there should ne some persistence aka backend, drag and drop and a single item in Doing allowed." []
                , newTask "Finish kandoro" "Ideally there should ne some persistence aka backend, drag and drop and a single item in Doing allowed." []
                , newTask "Finish kandoro" "Ideally there should ne some persistence aka backend, drag and drop and a single item in Doing allowed." []
                , newTask "Finish kandoro" "Ideally there should ne some persistence aka backend, drag and drop and a single item in Doing allowed." []
                ]
            , displayList KandoroTask.Doing []
            , displayList KandoroTask.Done []
            , displayList KandoroTask.Blocked []
            ]
        ]


displayList : KandoroTask.State -> List KTask -> Html Msg
displayList state tasks =
    let
        stateAsString =
            KandoroTask.stateToString state
    in
    div [ class <| "board--column board--column__" ++ String.toLower stateAsString ]
        [ header []
            [ text stateAsString ]
        , displayTasksInList tasks
        , footer [] [ text "Add a task" ]
        ]


displayTasksInList : List KTask -> Html Msg
displayTasksInList tasks =
    ul [] (List.map displayTaskAsListItem tasks)


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
