module NoUnsafePorts exposing
    ( rule
    , any, onlyIncomingPorts, Check
    )

{-|

@docs rule


## Config

@docs any, onlyIncomingPorts, Check

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Forbid unsafe types in ports.

    config : List Rule
    config =
        [ NoUnsafePorts.rule NoUnsafePorts.any
        ]

This rule reports any ports that do not send/receive a [`Json.Encode.Value`][Json.Encode.Value].

[Json.Encode.Value]: https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value


## Why is this a problem?

If a port expecting an `Int` receives a `Float` it will cause a runtime error. We can prevent this by just wrapping the incoming data as `Json.Encode.Value` and handling the data through a [`Decoder`][Json.Decode] instead. This guarantees that your application provides some sort of error handling. This is discussed in the Elm guide under [Verifying Flags].

[Json.Decode]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
[Verifying Flags]: https://guide.elm-lang.org/interop/flags.html#verifying-flags


## Success

    port action : (Json.Decode.Value -> msg) -> Sub msg

    port alarm : Json.Encode.Value -> Cmd msg


## Failure

    port action : (Int -> msg) -> Sub msg

    port alarm : String -> Cmd msg


## Caveats

  - Outgoing ports must accept a `Value` from `Json.Encode` or `Json.Decode` and result in a `Cmd`.
  - Incoming ports must expect a `Value` from `Json.Encode` or `Json.Decode` and result in a `Sub`.
  - The rule looks for the types `Cmd`, `Sub` and `Value` only - do not alias these types.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template sparksp/elm-review-ports/example --rules NoUnsafePorts
```

-}
rule : Check -> Rule
rule check =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnsafePorts" (initialModuleContext check)
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.fromModuleRuleSchema


{-| Check both incoming and outgoing ports.

    config : List Rule
    config =
        [ NoUnsafePorts.rule NoUnsafePorts.any
        ]

This is the option you will want most of the time.

-}
any : Check
any =
    CheckAll


{-| Check incoming ports only.

    config : List Rule
    config =
        [ NoUnsafePorts.rule NoUnsafePorts.onlyIncomingPorts
        ]

Use this option if you want to allow basic types in outgoing ports.

-}
onlyIncomingPorts : Check
onlyIncomingPorts =
    Check IncomingPort


{-| Check type.
-}
type Check
    = CheckAll
    | Check PortType



--- VISITORS


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor nodes context =
    ( fastConcatMap (checkDeclaration context) nodes, context )


checkDeclaration : ModuleContext -> Node Declaration -> List (Rule.Error {})
checkDeclaration context (Node _ declaration) =
    case declaration of
        Declaration.PortDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.FunctionTypeAnnotation portArguments (Node _ portReturnType) ->
                    checkPort context (Node.value name) portArguments (getPortType portReturnType)

                _ ->
                    []

        _ ->
            []


checkPort : ModuleContext -> String -> Node TypeAnnotation -> Maybe PortType -> List (Rule.Error {})
checkPort ((Context { check }) as context) name portArguments maybePortType =
    case Maybe.map (\portType -> ( portType, canCheck check portType )) maybePortType of
        Just ( IncomingPort, True ) ->
            checkIncomingPort context name (Node.value portArguments)

        Just ( OutgoingPort, True ) ->
            checkOutgoingPort context name portArguments

        Just ( _, False ) ->
            -- Config disabled this check
            []

        Nothing ->
            -- There is something off about this port declaration. The compiler will complain about this.
            []


checkIncomingPort : ModuleContext -> String -> TypeAnnotation -> List (Rule.Error {})
checkIncomingPort context name portArguments =
    case portArguments of
        TypeAnnotation.FunctionTypeAnnotation subMessageType _ ->
            checkPortArguments context (unsafeIncomingPortError name) subMessageType

        _ ->
            -- There is something off about this port declaration. The compiler will complain about this
            []


checkOutgoingPort : ModuleContext -> String -> Node TypeAnnotation -> List (Rule.Error {})
checkOutgoingPort context name portArguments =
    checkPortArguments context (unsafeOutgoingPortError name) portArguments


checkPortArguments : ModuleContext -> (Range -> String -> Rule.Error {}) -> Node TypeAnnotation -> List (Rule.Error {})
checkPortArguments context makeError ((Node range portArguments) as portArgumentsNode) =
    case portArguments of
        TypeAnnotation.Typed portType _ ->
            let
                functionCall : String
                functionCall =
                    Node.value portType |> Tuple.second
            in
            case expandFunctionCall context portArgumentsNode functionCall of
                ( [ "Json", "Decode" ], "Value" ) ->
                    []

                ( [ "Json", "Encode" ], "Value" ) ->
                    []

                expandedPortType ->
                    [ makeError range (formatType expandedPortType) ]

        TypeAnnotation.Record _ ->
            [ makeError range "record" ]

        TypeAnnotation.Tupled _ ->
            [ makeError range "tuple" ]

        _ ->
            [ makeError range "type" ]


getPortType : TypeAnnotation -> Maybe PortType
getPortType portReturnType =
    case portReturnType of
        TypeAnnotation.Typed (Node _ typed) _ ->
            case typed of
                ( [], "Sub" ) ->
                    Just IncomingPort

                ( [], "Cmd" ) ->
                    Just OutgoingPort

                _ ->
                    Nothing

        _ ->
            Nothing


type PortType
    = IncomingPort
    | OutgoingPort


type ModuleContext
    = Context
        { check : Check
        , lookupTable : ModuleNameLookupTable
        }


canCheck : Check -> PortType -> Bool
canCheck check portType =
    case check of
        CheckAll ->
            True

        Check thisType ->
            thisType == portType


initialModuleContext : Check -> Rule.ContextCreator () ModuleContext
initialModuleContext check =
    Rule.initContextCreator
        (\lookupTable () ->
            Context
                { check = check
                , lookupTable = lookupTable
                }
        )
        |> Rule.withModuleNameLookupTable


expandFunctionCall : ModuleContext -> Node TypeAnnotation -> String -> ( ModuleName, String )
expandFunctionCall (Context { lookupTable }) typeAnnotation functionCall =
    ( ModuleNameLookupTable.moduleNameFor lookupTable typeAnnotation
        |> Maybe.withDefault []
    , functionCall
    )


unsafeIncomingPortError : String -> Range -> String -> Rule.Error {}
unsafeIncomingPortError name range portType =
    Rule.error
        { message = "Port `" ++ name ++ "` expects unsafe " ++ portType ++ " data."
        , details =
            [ "When a port expecting a basic type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Encode.Value` and write a `Decoder` handle the data."
            ]
        }
        range


unsafeOutgoingPortError : String -> Range -> String -> Rule.Error {}
unsafeOutgoingPortError name range portType =
    Rule.error
        { message = "Port `" ++ name ++ "` sends unsafe " ++ portType ++ " data."
        , details =
            [ "When a port expecting an unsafe type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Encode.Value` and use an `Encoder` to generate a safe value."
            ]
        }
        range


formatType : ( ModuleName, String ) -> String
formatType type_ =
    let
        ( moduleName, name ) =
            filterCoreType type_
    in
    "`" ++ String.join "." (moduleName ++ [ name ]) ++ "`"


filterCoreType : ( ModuleName, String ) -> ( ModuleName, String )
filterCoreType type_ =
    case type_ of
        ( [ "Basics" ], "Bool" ) ->
            ( [], "Bool" )

        ( [ "Basics" ], "Float" ) ->
            ( [], "Float" )

        ( [ "Basics" ], "Int" ) ->
            ( [], "Int" )

        ( [ "String" ], "String" ) ->
            ( [], "String" )

        _ ->
            type_



--- FASTER LIST OPERATIONS


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn =
    let
        helper : a -> List b -> List b
        helper item acc =
            fn item ++ acc
    in
    List.foldr helper []
