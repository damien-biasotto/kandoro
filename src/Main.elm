module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, text, ul , li)
import Html.Events exposing (onClick)
import Time exposing (utc)
import Task
import Url
import Timer as T exposing (Duration, Msg, Timer, newTimer, update)


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
    ( { timer = (T.newTimer (T.Duration 2) (T.Duration 2) (T.Duration 1)), timezone =  utc, key = key, url = url }
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
            ( {model | timezone = zone} , Cmd.none)
        Tick _ ->
            ( { model | timer = Tuple.first (T.update <| T.Update model.timer) }, Task.perform AddTimestamp Time.now)
        StartTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.Start timer) },Task.perform AddTimestamp Time.now)
        PauseTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.Pause timer) }, Task.perform AddTimestamp Time.now)
        RestartTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.Restart timer) }, Task.perform AddTimestamp Time.now)
        EndTimer timer ->
            ( { model | timer = Tuple.first (T.update <| T.End timer) }, Task.perform AddTimestamp Time.now)
        AddTimestamp time ->
            ( {model | timer = Tuple.first (T.update <| (T.TimestampTransition model.timer time)) }, Cmd.none)

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


view : Model -> Browser.Document Msg
view model =
    { title = "Kandoro - The Kanban Pomodoro App"
    , body =  [
        div []
        [ h1 [] [ text <| T.toString model.timer ]
        , div [] [
            ul [] (li [] [text <| T.getTotalDuration model.timer] :: List.map viewTransition (T.getTransitions model.timer))
          ]
        , button [ onClick <| StartTimer model.timer ] [ text "Start" ]
        , button [ onClick <| PauseTimer model.timer ] [ text "Pause" ]
        , button [ onClick <| RestartTimer model.timer ] [ text "Reset" ]
        , button [ onClick <| EndTimer model.timer ] [ text "End" ]
        ]
    ]
    }


viewTransition : T.Transition -> Html Msg
viewTransition transition =
    li [] [text <| T.displayTransition transition ]
