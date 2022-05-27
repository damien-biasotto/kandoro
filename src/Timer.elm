module Timer exposing (Duration(..), Msg(..), State(..), Timer, Transition(..), displayTransition, getCurrentValue, getLongBreakDuration, getMaximumDuration, getShortBreakDuration, getState, getStats, getTransitions, newTimer, toString, update)

import Iso8601 exposing (fromTime)
import Time exposing (millisToPosix, toHour, toMinute, toSecond, utc)


type Duration
    = Duration Int



{- All possible states for a timer -}


type State
    = NotStarted
    | Running
    | LongBreak
    | ShortBreak
    | Paused
    | Ended
    | Reset


type Transition
    = TransitionWithoutTime ( State, State )
    | TransitionWithTime ( State, State, Time.Posix )


type Timer
    = Timer
        { state : State
        , currentValue : Int
        , maximumDuration : Duration
        , longBreakDuration : Duration
        , shortBreakDuration : Duration
        , transitions : List Transition
        }


type Msg
    = Start Timer
    | Update Timer
    | Pause Timer
    | Restart Timer
    | End Timer
    | TimestampTransition Timer Time.Posix


newTimer : Duration -> Duration -> Duration -> Timer
newTimer duration long small =
    Timer
        { state = NotStarted
        , currentValue = 0
        , maximumDuration = duration
        , longBreakDuration = long
        , shortBreakDuration = small
        , transitions = []
        }


resetTimer : Timer -> Timer
resetTimer (Timer timer) =
    let
        oldState =
            .state timer
    in
    Timer
        { timer
            | state = Reset
            , currentValue = 0
            , transitions = TransitionWithoutTime ( oldState, Reset ) :: timer.transitions
        }


startTimer : Timer -> Timer
startTimer (Timer timer) =
    let
        oldState =
            .state timer
    in
    Timer
        { timer
            | state = Running
            , transitions = TransitionWithoutTime ( oldState, Running ) :: timer.transitions
        }


pauseTimer : Timer -> Timer
pauseTimer (Timer timer) =
    let
        oldState =
            .state timer
    in
    Timer
        { timer
            | state = Paused
            , shortBreakDuration = timer.shortBreakDuration
            , transitions = TransitionWithoutTime ( oldState, Paused ) :: timer.transitions
        }


endTimer : Timer -> Timer
endTimer (Timer timer) =
    let
        oldState =
            .state timer
    in
    Timer
        { timer
            | state = Ended
            , transitions = TransitionWithoutTime ( oldState, Ended ) :: timer.transitions
        }


getDurationInSeconds : Duration -> Int
getDurationInSeconds (Duration int) =
    int


getNextTypeOfBreak : List Transition -> State
getNextTypeOfBreak transitions =
    case transitions of
        [] ->
            ShortBreak

        _ ->
            let
                amountOfShortbreak =
                    List.length <| List.filter (filterTransitionByState ShortBreak) transitions

                amountOfLongbreak =
                    List.length <| List.filter (filterTransitionByState LongBreak) transitions

                amountOfPauses =
                    amountOfShortbreak + amountOfLongbreak
            in
            if amountOfPauses > 0 && modBy 4 amountOfPauses == 0 then
                LongBreak

            else
                ShortBreak


update : Msg -> ( Timer, Cmd Msg )
update msg =
    case msg of
        Start timer ->
            ( startTimer timer, Cmd.none )

        Pause timer ->
            ( pauseTimer timer, Cmd.none )

        Restart timer ->
            ( resetTimer timer, Cmd.none )

        Update timer ->
            ( updateTimer timer, Cmd.none )

        End timer ->
            ( endTimer timer, Cmd.none )

        TimestampTransition timer time ->
            ( timeStampLatestTransition timer time, Cmd.none )


updateTimer : Timer -> Timer
updateTimer (Timer timer) =
    case timer.state of
        ShortBreak ->
            if timer.currentValue == getDurationInSeconds timer.shortBreakDuration then
                updateTimerStateAndTransitions (Timer timer) Running 0

            else
                updateTimerStateAndTransitions (Timer timer) ShortBreak (timer.currentValue + 1)

        LongBreak ->
            if timer.currentValue == getDurationInSeconds timer.longBreakDuration then
                updateTimerStateAndTransitions (Timer timer) Running 0

            else
                updateTimerStateAndTransitions (Timer timer) LongBreak (timer.currentValue + 1)

        Running ->
            let
                nextBreak =
                    getNextTypeOfBreak timer.transitions
            in
            if timer.currentValue == getDurationInSeconds timer.maximumDuration then
                updateTimerStateAndTransitions
                    (Timer timer)
                    nextBreak
                    0

            else
                updateTimerStateAndTransitions (Timer timer) Running (timer.currentValue + 1)

        _ ->
            Timer timer


timeStampLatestTransition : Timer -> Time.Posix -> Timer
timeStampLatestTransition (Timer timer) time =
    case timer.transitions of
        [] ->
            Timer timer

        (TransitionWithTime _) :: _ ->
            Timer timer

        (TransitionWithoutTime ( from, to )) :: _ ->
            Timer
                { timer
                    | transitions =
                        TransitionWithTime ( from, to, time ) :: Maybe.withDefault [] (List.tail timer.transitions)
                }


updateTimerStateAndTransitions : Timer -> State -> Int -> Timer
updateTimerStateAndTransitions (Timer timer) newState value =
    let
        transitions =
            if newState /= timer.state then
                TransitionWithoutTime ( timer.state, newState ) :: timer.transitions

            else
                timer.transitions
    in
    Timer
        { timer
            | state = newState
            , currentValue = value
            , transitions = transitions
        }


stateToString : State -> String
stateToString state =
    case state of
        NotStarted ->
            "Not started"

        Running ->
            "Running"

        LongBreak ->
            "Long break"

        ShortBreak ->
            "Short break"

        Paused ->
            "Paused"

        Ended ->
            "Ended"

        Reset ->
            "Reset"


toHumanTime : Time.Posix -> String
toHumanTime time =
    (String.padLeft 2 '0' <|
        String.fromInt <|
            toHour utc time
    )
        ++ ":"
        ++ (String.padLeft 2 '0' <|
                String.fromInt <|
                    toMinute utc time
           )
        ++ ":"
        ++ (String.padLeft 2 '0' <|
                String.fromInt <|
                    toSecond utc time
           )


toString : Timer -> String
toString (Timer timer) =
    "Timer state: " ++ stateToString timer.state ++ " - " ++ toHumanTime (millisToPosix <| timer.currentValue * 1000)


displayTransition : Transition -> String
displayTransition transition =
    case transition of
        TransitionWithTime ( from, to, when ) ->
            "[" ++ fromTime when ++ "]" ++ " " ++ "Transitioned from " ++ stateToString from ++ " to " ++ stateToString to

        TransitionWithoutTime ( from, to ) ->
            "Transitioned from " ++ stateToString from ++ " to " ++ stateToString to


filterTransitionByState : State -> Transition -> Bool
filterTransitionByState state transition =
    case transition of
        TransitionWithTime ( from, _, _ ) ->
            from == state

        TransitionWithoutTime ( from, _ ) ->
            from == state


getStats : Timer -> String
getStats (Timer timer) =
    let
        completedRun =
            List.length <|
                List.filter
                    (filterTransitionByState Running)
                    timer.transitions

        completedShortbreak =
            List.length <| List.filter (filterTransitionByState ShortBreak) timer.transitions

        completedLongbreak =
            List.length <| List.filter (filterTransitionByState LongBreak) timer.transitions

        shortbreakDurationTimestamp =
            millisToPosix <|
                1000
                    * completedShortbreak
                    * getDurationInSeconds timer.shortBreakDuration

        longBreakDurationTimestamp =
            millisToPosix <|
                1000
                    * completedLongbreak
                    * getDurationInSeconds timer.longBreakDuration

        totalPauseDurationTimestamp =
            millisToPosix <|
                1000
                    * ((completedLongbreak * getDurationInSeconds timer.longBreakDuration)
                        + (completedShortbreak * getDurationInSeconds timer.shortBreakDuration)
                      )

        workingDurationTimestamp =
            millisToPosix <|
                1000
                    * completedRun
                    * getDurationInSeconds timer.maximumDuration

        manuallyPaused =
            List.length <| List.filter (filterTransitionByState Paused) timer.transitions
    in
    "Time spent working: "
        ++ toHumanTime workingDurationTimestamp
        ++ " -  "
        ++ "Number of completed cycles: "
        ++ String.fromInt completedRun
        ++ " - "
        ++ "Time spent on break: "
        ++ toHumanTime totalPauseDurationTimestamp
        ++ " - (# Short break : "
        ++ String.fromInt completedShortbreak
        ++ " - Total Short break duration: "
        ++ toHumanTime shortbreakDurationTimestamp
        ++ " - # Long break: "
        ++ String.fromInt completedLongbreak
        ++ " - Total long break duration: "
        ++ toHumanTime longBreakDurationTimestamp
        ++ " - # Manually paused: "
        ++ String.fromInt manuallyPaused
        ++ " - Total manually paused duration: "
        ++ ")"


getState : Timer -> State
getState (Timer timer) =
    timer.state


getMaximumDuration : Timer -> Duration
getMaximumDuration (Timer timer) =
    timer.maximumDuration


getLongBreakDuration : Timer -> Duration
getLongBreakDuration (Timer timer) =
    timer.longBreakDuration


getShortBreakDuration : Timer -> Duration
getShortBreakDuration (Timer timer) =
    timer.shortBreakDuration


getCurrentValue : Timer -> Int
getCurrentValue (Timer timer) =
    timer.currentValue


getTransitions : Timer -> List Transition
getTransitions (Timer timer) =
    timer.transitions
