module NoUnsafePortsTests exposing (all)

import Fuzz exposing (Fuzzer)
import NoUnsafePorts exposing (rule)
import Review.Test
import Test exposing (Test, describe, fuzz, test)


incomingPortTests : Test
incomingPortTests =
    describe "incoming ports"
        [ fuzz fuzzUnsafeType "unsafe type" <|
            \unsafeType ->
                portModule unsafeType
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unsafePortError { name = "action", type_ = unsafeType, under = unsafeType }
                        ]
        , fuzz fuzzMaybeType "maybe type" <|
            \maybeType ->
                portModule maybeType
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unsafePortError { name = "action", type_ = "Maybe", under = maybeType }
                        ]
        , fuzz fuzzListType "list type" <|
            \listType ->
                portModule listType
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unsafePortError { name = "action", type_ = "List", under = listType }
                        ]
        , fuzz fuzzArrayType "array type" <|
            \arrayType ->
                portModule arrayType
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unsafePortError { name = "action", type_ = "Array", under = arrayType }
                        ]
        , test "record type" <|
            \_ ->
                portModule "{ message : String }"
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unsafePortError { name = "action", type_ = "record", under = "{ message : String }" }
                        ]
        , test "tuple type" <|
            \_ ->
                portModule "( String, Int )"
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ unsafePortError { name = "action", type_ = "tuple", under = "( String, Int )" }
                        ]
        , test "Json.Decode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Decode
port action : (Json.Decode.Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "Json.Encode.Value type" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode
port action : (Json.Encode.Value -> msg) -> Sub msg
main = 1"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , Test.todo "exposed Json.Decode.Value type"
        , Test.todo "exposed Json.Encode.Value type"
        , Test.todo "aliased Json.Decode.Value type"
        , Test.todo "aliased Json.Encode.Value type"
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


outgoingPortTests : Test
outgoingPortTests =
    Test.todo "outgoing ports"


all : Test
all =
    describe "NoUnsafePorts"
        [ incomingPortTests
        , outgoingPortTests
        ]


unsafePortError : { name : String, type_ : String, under : String } -> Review.Test.ExpectedError
unsafePortError { name, type_, under } =
    Review.Test.error
        { message = "Port `" ++ name ++ "` expects unsafe " ++ type_ ++ " data."
        , details =
            [ "When a port expecting an unsafe type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Decode.Value` and use a `Decoder` result to handle any mismatched type errors."
            ]
        , under = under
        }


portModule : String -> String
portModule portType =
    String.join "\n"
        [ "module Main exposing (main)"
        , "port action : (" ++ portType ++ " -> msg) -> Sub msg"
        , "main = 1"
        ]
