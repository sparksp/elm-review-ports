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
                          , [ unusedPortError "alarm" |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 11 } }
                            , unusedPortError "action" |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 12 } }
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
                        ]
        , test "do not report outgoing ports that are used" <|
            \_ ->
                """
port module Ports exposing (play)
port alarm : String -> Cmd msg
play = alarm "play"
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
                        ]
        , test "do not report incoming ports that are used" <|
            \_ ->
                """
port module Ports exposing (a)
port action : (String -> msg) -> Sub msg
type ActionMsg = Action String
a = action Action
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


unusedPortError : String -> Review.Test.ExpectedError
unusedPortError name =
    Review.Test.error
        { message = "Port `" ++ name ++ "` is not used anywhere."
        , details = [ "TODO: This is a problem because..." ]
        , under = name
        }


all : Test
all =
    describe "NoUnusedPorts"
        [ applicationTests
        , moduleTests
        ]



--- HELPERS


portsModule : String
portsModule =
    """
port module Ports exposing (alarm, action)
port alarm : String -> Cmd msg
port action : (String -> msg) -> Sub msg
"""
