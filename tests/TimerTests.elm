module TimerTests exposing (timerTestSuite)

import Timer as T exposing (Timer, State, Duration, Transition, newTimer, update, getState, getMaximumDuration, getLongBreakDuration, getShortBreakDuration, getTransitions, getCurrentValue)
import Expect
import Fuzz exposing (..)
import Test exposing (..)
import List

createTimer : Int -> Int -> Int -> Timer
createTimer x y z =
    T.newTimer (T.Duration x) (T.Duration y) (T.Duration z)

startTimer : Timer -> Timer
startTimer timer =
    Tuple.first (update <| T.Start timer)

pauseTimer : Timer -> Timer
pauseTimer timer =
    Tuple.first (update <| T.Pause timer)

timerTestSuite : Test
timerTestSuite =
    describe "Timer operations"
        [ describe "Timer creation"
            [ test "a newly created timer should have a NotStarted state " <|
                \_ ->
                    let
                        timer = createTimer 0 0 0
                        in
                        Expect.equal (getState timer) T.NotStarted
            , test " a newly created timer should have no Transitions" <|
            \_ ->
                let
                    timer = createTimer 0 0 0
                in
                Expect.true "Transitions should be empty" (List.isEmpty (getTransitions timer))

            , test " a newly created tiner should start with 0" <|
            \_ ->
                let
                    timer = createTimer 0 0 0
                in
                Expect.true "Timer value should be equal to 0" (0 == (getCurrentValue timer))
            , test "Create a new timer with a custom maximum duration" <|
            \_ ->
                    let
                        timer = createTimer 1 0 0
                    in
                    Expect.equal (getMaximumDuration timer) (T.Duration 1)

            , test "Create a new timer with a custom long break duration" <|
            \_ ->
                let
                    timer = createTimer 0 1 0
                    in
                    Expect.equal (getLongBreakDuration timer) (T.Duration 1)

            , test "Create a new timer with a custom small break duration" <|
            \_ ->
                let
                    timer = createTimer 0 0 1
                    in
                    Expect.equal (getShortBreakDuration timer) (T.Duration 1)
            ]
        , describe "Time start"
            [ test "a started timer should have a running state" <|
            \_ ->
                let
                    timer = createTimer 1 1 1
                in
                    Expect.equal (getState <| startTimer timer) T.Running
            , test " a started timer should have a transition" <|
            \_ ->
                let
                    timer = startTimer <| createTimer 1 1 1
                in
                    Expect.false "A started timer should have a transition from NotStarted to Running" (List.isEmpty (getTransitions timer))

            , test " a started timer should have a transition from NotStarted to Running" <|
            \_ ->
                let
                    timer = startTimer <| createTimer 1 1 1
                    in
                    Expect.equal (Just <| T.TransitionWithoutTime (T.NotStarted, T.Running)) (List.head <| getTransitions timer)
            ]
        ]
