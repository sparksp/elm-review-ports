module Tests.NoUnusedPorts exposing (all)

import NoUnusedPorts exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


applicationTests : Test
applicationTests =
    describe "application tests"
        [ test "report when ports are unused" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
main = "Hello"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "alarm"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 11 } }
                            , unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report when ports are used from an import" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Json.Encode as Encode
import Ports
main = Ports.alarm (Encode.string "play")
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report ports used from an exposed import" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Json.Encode as Encode
import Ports exposing (alarm)
main = alarm (Encode.string "play")
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report ports used from an aliased module" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Json.Encode as Encode
import Ports as P
main = P.alarm (Encode.string "play")
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report ports used from an import exposing all" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Json.Encode as Encode
import Ports exposing (..)
main = alarm (Encode.string "play")
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "report ports used from an import but not exposed" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Json.Encode as Encode
import Ports
main = 1
unused = Ports.alarm (Encode.string "play")
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortWithCallersError "alarm" [ "Main.unused" ] |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 11 } }
                            , unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report ports used from an import but main is not exposed" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (a)
import Json.Encode as Encode
import Ports
main = Ports.alarm (Encode.string "play")
a = 1
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report ports used from an import when everything is exposed" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (..)
import Json.Encode as Encode
import Ports
main = Ports.alarm (Encode.string "play")
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report ports called via an unused private main" <|
            \_ ->
                [ portsModule
                , """
module Page.Alarm exposing (update)

import Json.Encode as Encode
import Ports

main = Ports.alarm (Encode.string "play")
update = main
"""
                , """
module Main exposing (main)
main = 1
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 5, column = 6 }, end = { row = 5, column = 12 } }
                            , unusedPortError "unused"
                            ]
                          )
                        ]
        , test "do not report port used via many modules" <|
            \_ ->
                [ portsModule
                , """
module Worker exposing (init, subscriptions, update)

import Json.Encode as Encode
import Ports


type Model
    = Count Int


type Msg
    = Up
    | Down
    | GotAction String


init : () -> Model
init () =
    Count 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, play )


play : Cmd Msg
play = Ports.alarm (Encode.string "play")


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.action GotAction
"""
                , """
module Utils exposing (add)

import Html


add : Int -> Int -> Int
add a b =
    a + b
"""
                , """
module Main exposing (main)

import Utils
import Worker


main =
    Platform.worker
        { init = Worker.init
        , update = Worker.update
        , subscriptions = Worker.subscriptions
        }
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "unused"
                            ]
                          )
                        ]
        ]


moduleTests : Test
moduleTests =
    describe "single module"
        [ test "report when an outgoing port is unused and not exposed" <|
            \_ ->
                """
port module Ports exposing (a)
import Json.Encode as Encode
port alarm : Encode.Value -> Cmd msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedPortError "alarm"
                        ]
        , test "do not report outgoing ports that are used" <|
            \_ ->
                """
port module Ports exposing (main)
import Json.Encode as Encode
port alarm : Encode.Value -> Cmd msg
load = alarm (Encode.string "load")
play = alarm (Encode.string "play")
stop = alarm (Encode.string "stop")
main = play
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "report when an incoming port is unused and not exposed" <|
            \_ ->
                """
port module Ports exposing (a)
import Json.Encode as Encode
port action : (Encode.Value -> msg) -> Sub msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedPortError "action"
                        ]
        , test "do not report incoming ports that are used" <|
            \_ ->
                """
port module Ports exposing (main)
import Json.Encode as Encode
port action : (Encode.Value -> msg) -> Sub msg
type ActionMsg = Action Encode.Value
main = action Action
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "report when a port is used but the caller is not exposed" <|
            \_ ->
                """
port module Ports exposing (a)
import Json.Encode as Encode
port alarm : Encode.Value -> msg
load = alarm (Encode.string "load")
play = alarm (Encode.string "play")
stop = alarm (Encode.string "stop")
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedPortWithCallersError "alarm" [ "Ports.load", "Ports.play", "Ports.stop" ]
                            |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 11 } }
                        ]
        , test "do not report when port is used but functions are defined out of sequence" <|
            \_ ->
                """
port module Ports exposing (main)
import Json.Encode as Encode
port alarm : Encode.Value -> msg
main = play
load = alarm (Encode.string "load")
play = alarm (Encode.string "play")
stop = alarm (Encode.string "stop")
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "does not crash when it encounters a recursive function call" <|
            \_ ->
                """
port module Main exposing (main)
import Json.Encode as Encode
main = update
update = ping "play"
ping cmd = pong (alarm cmd)
pong cmd = ping (alarm cmd)
port alarm : Encode.Value -> msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


all : Test
all =
    describe "NoUnusedPorts"
        [ applicationTests
        , moduleTests
        ]



--- HELPERS


unusedPortError : String -> Review.Test.ExpectedError
unusedPortError name =
    Review.Test.error
        { message = errorMessage name
        , details = errorDetails
        , under = name
        }


unusedPortWithCallersError : String -> List String -> Review.Test.ExpectedError
unusedPortWithCallersError name callers =
    Review.Test.error
        { message = errorMessage name
        , details = errorDetails ++ withCallers callers
        , under = name
        }


errorMessage : String -> String
errorMessage name =
    "Port `" ++ name ++ "` is never used (Warning: can cause JS runtime errors)"


errorDetails : List String
errorDetails =
    [ "Unused ports are not available in the compiled JavaScript and can cause runtime errors when you try to access them."
    , "You should either use this port somewhere, or remove it at the location I pointed at. This may highlight some other unused code in your project too."
    , "Warning: If you remove this port, remember to remove any calls to it in your JavaScript code too."
    ]


withCallers : List String -> List String
withCallers callers =
    [ "I found this port called by the following functions, but none of them trace back to a `main` function:"
    , String.join "\n" <| List.map (\caller -> "-> " ++ caller) callers
    ]


portsModule : String
portsModule =
    """
port module Ports exposing (alarm, action)
import Json.Encode as Encode
port alarm : Encode.Value -> Cmd msg
port action : (Encode.Value -> msg) -> Sub msg
port unused : Encode.Value -> Cmd msg
"""
