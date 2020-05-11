module NoUnsafePortsTests exposing (all)

import Fuzz exposing (Fuzzer)
import NoUnsafePorts exposing (onlyIncomingPorts, rule)
import Review.Test
import Test exposing (Test, describe, fuzz, test)


all : Test
all =
    describe "NoUnsafePorts"
        [ configTests
        , incomingPortTests
        , outgoingPortTests
        ]


configTests : Test
configTests =
    describe "configuration"
        [ test "any" <|
            \_ ->
                """
port module Main exposing (main)
port action : (Int -> msg) -> Sub msg
port alarm : String -> Cmd msg
main = 1 """
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = quote "Int", under = "Int" }
                        , unsafeOutgoingPortError { name = "alarm", type_ = quote "String", under = "String" }
                        ]
        , test "onlyIncomingPorts" <|
            \_ ->
                """
port module Main exposing (main)
port action : (Int -> msg) -> Sub msg
port alarm : String -> Cmd msg
main = 1 """
                    |> Review.Test.run (rule onlyIncomingPorts)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = quote "Int", under = "Int" }
                        ]
        ]


incomingPortTests : Test
incomingPortTests =
    describe "incoming ports"
        [ fuzz fuzzUnsafeType "unsafe type" <|
            \unsafeType ->
                incomingPortModule unsafeType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = quote unsafeType, under = unsafeType }
                        ]
        , fuzz fuzzMaybeType "maybe type" <|
            \maybeType ->
                incomingPortModule maybeType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = quote "Maybe", under = maybeType }
                        ]
        , fuzz fuzzListType "list type" <|
            \listType ->
                incomingPortModule listType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = quote "List", under = listType }
                        ]
        , fuzz fuzzArrayType "array type" <|
            \arrayType ->
                incomingPortModule arrayType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = quote "Array", under = arrayType }
                        ]
        , test "record type" <|
            \_ ->
                incomingPortModule "{ message : String }"
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = "record", under = "{ message : String }" }
                        ]
        , test "tuple type" <|
            \_ ->
                incomingPortModule "( String, Int )"
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeIncomingPortError { name = "action", type_ = "tuple", under = "( String, Int )" }
                        ]
        , test "Json.Decode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode
port action : (Json.Decode.Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "Json.Encode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode
port action : (Json.Encode.Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Decode exposing (Value) type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode exposing (Value)
port action : (Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Encode exposing (Value) type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode exposing (Value)
port action : (Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Decode exposing (..) Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode exposing (..)
port action : (Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Encode exposing (..) Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode exposing (..)
port action : (Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "aliased Json.Decode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode as D
port action : (D.Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "aliased Json.Encode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode as E
port action : (E.Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        ]


outgoingPortTests : Test
outgoingPortTests =
    describe "outgoing ports"
        [ fuzz fuzzUnsafeType "unsafe type" <|
            \unsafeType ->
                outgoingPortModule unsafeType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeOutgoingPortError { name = "action", type_ = quote unsafeType, under = unsafeType }
                        ]
        , fuzz fuzzMaybeType "maybe type" <|
            \maybeType ->
                outgoingPortModule maybeType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeOutgoingPortError { name = "action", type_ = quote "Maybe", under = maybeType }
                        ]
        , fuzz fuzzListType "list type" <|
            \listType ->
                outgoingPortModule listType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeOutgoingPortError { name = "action", type_ = quote "List", under = listType }
                        ]
        , fuzz fuzzArrayType "array type" <|
            \arrayType ->
                outgoingPortModule arrayType
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeOutgoingPortError { name = "action", type_ = quote "Array", under = arrayType }
                        ]
        , test "record type" <|
            \_ ->
                outgoingPortModule "{ message : String }"
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeOutgoingPortError { name = "action", type_ = "record", under = "{ message : String }" }
                        ]
        , test "tuple type" <|
            \_ ->
                outgoingPortModule "( String, Int )"
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectErrors
                        [ unsafeOutgoingPortError { name = "action", type_ = "tuple", under = "( String, Int )" }
                        ]
        , test "Json.Decode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode
port action : Json.Decode.Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "Json.Encode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode
port action : Json.Encode.Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Decode exposing (Value) type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode exposing (Value)
port action : Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Encode exposing (Value) type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode exposing (Value)
port action : Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Decode exposing (..) Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode exposing (..)
port action : Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "import Json.Encode exposing (..) Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode exposing (..)
port action : Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "aliased Json.Decode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode as D
port action : D.Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        , test "aliased Json.Encode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode as E
port action : E.Value -> Cmd msg
main = 1"""
                    |> Review.Test.run (rule NoUnsafePorts.any)
                    |> Review.Test.expectNoErrors
        ]


fuzzUnsafeType : Fuzzer String
fuzzUnsafeType =
    Fuzz.oneOf
        [ Fuzz.constant "Bool"
        , Fuzz.constant "Int"
        , Fuzz.constant "Float"
        , Fuzz.constant "String"
        ]


fuzzMaybeType : Fuzzer String
fuzzMaybeType =
    Fuzz.oneOf
        [ Fuzz.constant "Maybe Bool"
        , Fuzz.constant "Maybe Int"
        , Fuzz.constant "Maybe Float"
        , Fuzz.constant "Maybe String"
        ]


fuzzListType : Fuzzer String
fuzzListType =
    Fuzz.oneOf
        [ Fuzz.constant "List Bool"
        , Fuzz.constant "List Int"
        , Fuzz.constant "List Float"
        , Fuzz.constant "List String"
        ]


fuzzArrayType : Fuzzer String
fuzzArrayType =
    Fuzz.oneOf
        [ Fuzz.constant "Array Bool"
        , Fuzz.constant "Array Int"
        , Fuzz.constant "Array Float"
        , Fuzz.constant "Array String"
        ]


unsafeIncomingPortError : { name : String, type_ : String, under : String } -> Review.Test.ExpectedError
unsafeIncomingPortError { name, type_, under } =
    Review.Test.error
        { message = "Port `" ++ name ++ "` expects unsafe " ++ type_ ++ " data."
        , details =
            [ "When a port expecting an unsafe type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Decode.Value` and use a `Decoder` result to handle any mismatched type errors."
            ]
        , under = under
        }


unsafeOutgoingPortError : { name : String, type_ : String, under : String } -> Review.Test.ExpectedError
unsafeOutgoingPortError =
    unsafeIncomingPortError


incomingPortModule : String -> String
incomingPortModule portType =
    String.join "\n"
        [ "module Main exposing (main)"
        , "port action : (" ++ portType ++ " -> msg) -> Sub msg"
        , "main = 1"
        ]


outgoingPortModule : String -> String
outgoingPortModule portType =
    String.join "\n"
        [ "module Main exposing (main)"
        , "port action : " ++ portType ++ " -> Cmd msg"
        , "main = 1"
        ]


quote : String -> String
quote code =
    "`" ++ code ++ "`"
