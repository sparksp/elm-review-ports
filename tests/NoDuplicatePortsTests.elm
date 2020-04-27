module NoDuplicatePortsTests exposing (all)

import NoDuplicatePorts exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


errorUnder : String -> { message : String, details : List String, under : String }
errorUnder portName =
    { message = String.concat [ "Another port named `", portName, "` already exists." ]
    , details = [ "When there are multiple ports with the same name you may encounter a JavaScript runtime error." ]
    , under = portName
    }


all : Test
all =
    describe "NoDuplicatePorts"
        [ test "should not report when there are no ports" <|
            \_ ->
                """
module A exposing (a)
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report when there is only file with ports" <|
            \_ ->
                """
port module A exposing (a)
port send : String -> Cmd msg
port recv : (String -> msg) -> Sub msg
a = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when there are two modules naming the same port" <|
            \_ ->
                [ """
port module A exposing (a)
port send : String -> Cmd msg
port recv : (String -> msg) -> Sub msg
a = 1""", """
port module B exposing (b)
port send : String -> Cmd msg
port recv : (String -> msg) -> Sub msg
b = 1""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error (errorUnder "send")
                            , Review.Test.error (errorUnder "recv")
                            ]
                          )
                        , ( "B"
                          , [ Review.Test.error (errorUnder "send")
                            , Review.Test.error (errorUnder "recv")
                            ]
                          )
                        ]
        ]
