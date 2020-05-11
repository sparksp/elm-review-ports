module NoUnsafePorts exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import as Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid unsafe types in ports.

    config : List Rule
    config =
        [ NoUnsafePorts.rule
        ]

This rule reports any ports that do not send/receive a `Json.Value`.


## Why is this a problem?

If a port expecting an `Int` receives a `Float` it will cause a runtime error. We can prevent this by just wrapping the incoming data as `Json.Encode.Value` and handling the data through a `Decoder` instead. This guarantees that your application provides some sort of error handling.


## Success

    port action : (Json.Decode.Value -> msg) -> Sub msg

    port alarm : Json.Encode.Value -> Cmd msg


## Failure

    port action : (Int -> msg) -> Sub msg

    port alarm : String -> Cmd msg

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnsafePorts" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.PortDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.FunctionTypeAnnotation portArguments portReturnType ->
                    case Node.value portReturnType of
                        TypeAnnotation.Typed (Node _ ( [], "Sub" )) _ ->
                            case Node.value portArguments of
                                TypeAnnotation.FunctionTypeAnnotation subMessageType _ ->
                                    case Node.value subMessageType of
                                        TypeAnnotation.Typed (Node _ ( [ "Json", "Decode" ], "Value" )) _ ->
                                            []

                                        TypeAnnotation.Typed (Node _ ( [ "Json", "Encode" ], "Value" )) _ ->
                                            []

                                        TypeAnnotation.Typed portType _ ->
                                            [ unsafePortError (Node.value name) (portType |> Node.value |> formatType) (Node.range subMessageType) ]

                                        TypeAnnotation.Record _ ->
                                            [ unsafePortError (Node.value name) "record" (Node.range subMessageType) ]

                                        TypeAnnotation.Tupled _ ->
                                            [ unsafePortError (Node.value name) "tuple" (Node.range subMessageType) ]

                                        _ ->
                                            [ unsafePortError (Node.value name) "type" (Node.range subMessageType) ]

                                _ ->
                                    []

                        TypeAnnotation.Typed (Node _ ( [], "Cmd" )) _ ->
                            []

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


unsafePortError : String -> String -> Range -> Error {}
unsafePortError name portType range =
    Rule.error
        { message = "Port `" ++ name ++ "` expects unsafe " ++ portType ++ " data."
        , details =
            [ "When a port expecting an unsafe type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Decode.Value` and use a `Decoder` result to handle any mismatched type errors."
            ]
        }
        range


formatType : ( ModuleName, String ) -> String
formatType ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])
