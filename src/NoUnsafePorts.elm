module NoUnsafePorts exposing
    ( rule
    , any, onlyIncomingPorts, Check
    )

{-|

@docs rule


## Config

@docs any, onlyIncomingPorts, Check

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
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

-}
rule : Check -> Rule
rule check =
    Rule.newModuleRuleSchema "NoUnsafePorts" (initialModuleContext check)
        |> Rule.withImportVisitor importVisitor
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


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor node context =
    ( [], rememberImportedModule (Node.value node) context )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor nodes context =
    ( fastConcatMap (checkDeclaration context) nodes, context )


checkDeclaration : ModuleContext -> Node Declaration -> List (Rule.Error {})
checkDeclaration context node =
    case Node.value node of
        Declaration.PortDeclaration { name, typeAnnotation } ->
            case Node.value typeAnnotation of
                TypeAnnotation.FunctionTypeAnnotation portArguments portReturnType ->
                    checkPort context (Node.value name) portArguments (getPortType portReturnType)

                _ ->
                    []

        _ ->
            []


checkPort : ModuleContext -> String -> Node TypeAnnotation -> Maybe PortType -> List (Rule.Error {})
checkPort ((Context { check }) as context) name portArguments maybePortType =
    case Maybe.map (\portType -> ( portType, canCheck check portType )) maybePortType of
        Just ( IncomingPort, True ) ->
            checkIncomingPort context name portArguments

        Just ( OutgoingPort, True ) ->
            checkOutgoingPort context name portArguments

        Just ( _, False ) ->
            -- Config disabled this check
            []

        Nothing ->
            -- There is something off about this port declaration. The compiler will complain about this.
            []


checkIncomingPort : ModuleContext -> String -> Node TypeAnnotation -> List (Rule.Error {})
checkIncomingPort context name portArguments =
    case Node.value portArguments of
        TypeAnnotation.FunctionTypeAnnotation subMessageType _ ->
            checkPortArguments context (unsafeIncomingPortError name) subMessageType

        _ ->
            -- There is something off about this port declaration. The compiler will complain about this
            []


checkOutgoingPort : ModuleContext -> String -> Node TypeAnnotation -> List (Rule.Error {})
checkOutgoingPort context name portArguments =
    checkPortArguments context (unsafeOutgoingPortError name) portArguments


checkPortArguments : ModuleContext -> (Node String -> Rule.Error {}) -> Node TypeAnnotation -> List (Rule.Error {})
checkPortArguments context makeError portArguments =
    case Node.value portArguments of
        TypeAnnotation.Typed portType _ ->
            case expandFunctionCall context (Node.value portType) of
                ( [ "Json", "Decode" ], "Value" ) ->
                    []

                ( [ "Json", "Encode" ], "Value" ) ->
                    []

                expandedPortType ->
                    [ makeError (Node.map (\_ -> formatType expandedPortType) portArguments) ]

        TypeAnnotation.Record _ ->
            [ makeError (Node.map (\_ -> "record") portArguments) ]

        TypeAnnotation.Tupled _ ->
            [ makeError (Node.map (\_ -> "tuple") portArguments) ]

        _ ->
            [ makeError (Node.map (\_ -> "type") portArguments) ]


getPortType : Node TypeAnnotation -> Maybe PortType
getPortType portReturnType =
    case Node.value portReturnType of
        TypeAnnotation.Typed node _ ->
            case Node.value node of
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
        { aliases : Dict ModuleName ModuleName
        , check : Check
        , imports : Dict String ModuleName
        }


canCheck : Check -> PortType -> Bool
canCheck check portType =
    case check of
        CheckAll ->
            True

        Check thisType ->
            thisType == portType


initialModuleContext : Check -> ModuleContext
initialModuleContext check =
    Context
        { aliases = Dict.empty
        , check = check
        , imports = Dict.empty
        }


rememberImportedModule : Import -> ModuleContext -> ModuleContext
rememberImportedModule { moduleName, moduleAlias, exposingList } context =
    let
        moduleNameValue : ModuleName
        moduleNameValue =
            Node.value moduleName
    in
    case moduleNameValue of
        "Json" :: _ ->
            context
                |> rememberImportedAlias moduleNameValue moduleAlias
                |> rememberImportedList moduleNameValue exposingList

        _ ->
            context


rememberImportedAlias : ModuleName -> Maybe (Node ModuleName) -> ModuleContext -> ModuleContext
rememberImportedAlias moduleName maybeModuleAlias (Context context) =
    case Maybe.map Node.value maybeModuleAlias of
        Just moduleAlias ->
            Context { context | aliases = Dict.insert moduleAlias moduleName context.aliases }

        Nothing ->
            Context context


rememberImportedList : ModuleName -> Maybe (Node Exposing) -> ModuleContext -> ModuleContext
rememberImportedList moduleName exposingList context =
    case Maybe.map Node.value exposingList of
        Just (Exposing.All _) ->
            rememberImportedAllModule moduleName context

        Just (Exposing.Explicit list) ->
            rememberImportedExplicitList moduleName list context

        Nothing ->
            context


rememberImportedAllModule : ModuleName -> ModuleContext -> ModuleContext
rememberImportedAllModule moduleName context =
    case moduleName of
        [ "Json", "Decode" ] ->
            rememberImport ( moduleName, "Value" ) context

        [ "Json", "Encode" ] ->
            rememberImport ( moduleName, "Value" ) context

        _ ->
            context


rememberImportedExplicitList : ModuleName -> List (Node Exposing.TopLevelExpose) -> ModuleContext -> ModuleContext
rememberImportedExplicitList moduleName list context =
    List.foldl (rememberImportedItem moduleName) context list


rememberImportedItem : ModuleName -> Node Exposing.TopLevelExpose -> ModuleContext -> ModuleContext
rememberImportedItem moduleName item context =
    case Node.value item of
        Exposing.FunctionExpose _ ->
            context

        Exposing.InfixExpose _ ->
            context

        Exposing.TypeOrAliasExpose name ->
            rememberImport ( moduleName, name ) context

        Exposing.TypeExpose { name } ->
            rememberImport ( moduleName, name ) context


rememberImport : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImport ( moduleName, name ) (Context context) =
    Context { context | imports = Dict.insert name moduleName context.imports }


expandFunctionCall : ModuleContext -> ( ModuleName, String ) -> ( ModuleName, String )
expandFunctionCall (Context { aliases, imports }) ( moduleCall, functionCall ) =
    let
        expandedModule : ModuleName
        expandedModule =
            case moduleCall of
                [] ->
                    lookupFunctionModule imports functionCall

                _ ->
                    lookupModuleAlias aliases moduleCall
    in
    ( expandedModule, functionCall )


lookupFunctionModule : Dict String ModuleName -> String -> ModuleName
lookupFunctionModule imports function =
    Dict.get function imports
        |> Maybe.withDefault []


lookupModuleAlias : Dict ModuleName ModuleName -> ModuleName -> ModuleName
lookupModuleAlias aliases moduleName =
    Dict.get moduleName aliases
        |> Maybe.withDefault moduleName


unsafeIncomingPortError : String -> Node String -> Rule.Error {}
unsafeIncomingPortError name portType =
    Rule.error
        { message = "Port `" ++ name ++ "` expects unsafe " ++ Node.value portType ++ " data."
        , details =
            [ "When a port expecting a basic type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Encode.Value` and write a `Decoder` handle the data."
            ]
        }
        (Node.range portType)


unsafeOutgoingPortError : String -> Node String -> Rule.Error {}
unsafeOutgoingPortError name portType =
    Rule.error
        { message = "Port `" ++ name ++ "` sends unsafe " ++ Node.value portType ++ " data."
        , details =
            [ "When a port expecting an unsafe type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Encode.Value` and use an `Encoder` to generate a safe value."
            ]
        }
        (Node.range portType)


formatType : ( ModuleName, String ) -> String
formatType ( moduleName, name ) =
    "`" ++ String.join "." (moduleName ++ [ name ]) ++ "`"



--- FASTER LIST OPERATIONS


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn list =
    List.foldr (fn >> (++)) [] list
