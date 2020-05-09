module NoUnusedPortsTests exposing (all)

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
                                |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }
                                |> Review.Test.whenFixed portsModuleWithoutAlarm
                            , unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "do not report when ports are used from an import" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Ports
main = Ports.alarm "play"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "do not report ports used from an exposed import" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Ports exposing (alarm)
main = alarm "play"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "do not report ports used from an aliased module" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Ports as P
main = P.alarm "play"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "do not report ports used from an import exposing all" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Ports exposing (..)
main = alarm "play"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "report ports used from an import but not exposed" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (main)
import Ports
main = 1
unused = Ports.alarm "play"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "alarm" |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }
                            , unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "report ports used from an import but main is not exposed" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (a)
import Ports
main = Ports.alarm "play"
a = 1
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "alarm" |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }
                            , unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "do not report ports used from an import when everything is exposed" <|
            \_ ->
                [ portsModule
                , """
module Main exposing (..)
import Ports
main = Ports.alarm "play"
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "Ports"
                          , [ unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "report ports called via an unused private main" <|
            \_ ->
                [ portsModule
                , """
module Page.Alarm exposing (update)

import Ports

main = Ports.alarm "play"
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
                          , [ unusedPortError "alarm" |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }
                            , unusedPortError "action"
                                |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
                                |> Review.Test.whenFixed portsModuleWithoutAction
                            , unusedPortError "unused"
                                |> Review.Test.whenFixed portsModuleWithoutUnused
                            ]
                          )
                        ]
        , test "do not report port used via many modules" <|
            \_ ->
                [ portsModule
                , """
module Worker exposing (init, subscriptions, update)

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
    ( model, Ports.alarm "play" )


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.action GotAction
"""
                , """
module Main exposing (main)

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
                                |> Review.Test.whenFixed portsModuleWithoutUnused
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
port alarm : String -> Cmd msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedPortError "alarm"
                            |> Review.Test.whenFixed
                                """
port module Ports exposing (a)

a = 1"""
                        ]
        , test "do not report outgoing ports that are used" <|
            \_ ->
                """
port module Ports exposing (main)
port alarm : String -> Cmd msg
load = alarm "load"
play = alarm "play"
stop = alarm "stop"
main = play
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "report when an incoming port is unused and not exposed" <|
            \_ ->
                """
port module Ports exposing (a)
port action : (String -> msg) -> Sub msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedPortError "action"
                            |> Review.Test.whenFixed
                                """
port module Ports exposing (a)

a = 1"""
                        ]
        , test "do not report incoming ports that are used" <|
            \_ ->
                """
port module Ports exposing (main)
port action : (String -> msg) -> Sub msg
type ActionMsg = Action String
main = action Action
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "report when a port is used but the caller is not exposed" <|
            \_ ->
                """
port module Ports exposing (a)
port alarm : String -> msg
load = alarm "load"
play = alarm "play"
stop = alarm "stop"
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unusedPortError "alarm" |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }
                        ]
        , test "do not report when port is used but functions are defined out of sequence" <|
            \_ ->
                """
port module Ports exposing (main)
port alarm : String -> msg
main = play
load = alarm "load"
play = alarm "play"
stop = alarm "stop"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "does not crash when it encounters a recursive function call" <|
            \_ ->
                """
port module Main exposing (main)
main = update
update = ping "play"
ping cmd = pong (alarm cmd)
pong cmd = ping (alarm cmd)
port alarm : String -> msg
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
        { message = "Port `" ++ name ++ "` is never used."
        , details =
            [ "You should either use this port somewhere, or remove it at the location I pointed at. This may highlight some other unused code in your project."
            , "Unused ports are not available in the compiled JavaScript and may cause runtime errors if you try to access them."
            , "Warning: If you remove this port, remember to remove any calls to it in your JavaScript code too."
            ]
        , under = name
        }


portsModule : String
portsModule =
    """
port module Ports exposing (alarm, action)
port alarm : String -> Cmd msg
port action : (String -> msg) -> Sub msg
port unused : String -> Cmd msg
"""


portsModuleWithoutAlarm : String
portsModuleWithoutAlarm =
    """
port module Ports exposing (alarm, action)

port action : (String -> msg) -> Sub msg
port unused : String -> Cmd msg
"""


portsModuleWithoutAction : String
portsModuleWithoutAction =
    """
port module Ports exposing (alarm, action)
port alarm : String -> Cmd msg

port unused : String -> Cmd msg
"""


portsModuleWithoutUnused : String
portsModuleWithoutUnused =
    """
port module Ports exposing (alarm, action)
port alarm : String -> Cmd msg
port action : (String -> msg) -> Sub msg

"""
