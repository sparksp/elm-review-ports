module NoUnsafePorts exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbid unsafe types in ports.

    config : List Rule
    config =
        [ NoUnsafePorts.rule
        ]

This rule reports any ports that do not send/receive a [`Json.Encode.Value`][Json.Encode.Value].

[Json.Encode.Value]: https://package.elm-lang.org/packages/elm/json/latest/Json-Encode#Value


## Why is this a problem?

If a port expecting an `Int` receives a `Float` it will cause a runtime error. We can prevent this by just wrapping the incoming data as `Json.Encode.Value` and handling the data through a [`Decoder`][Json.Decode] instead. This guarantees that your application provides some sort of error handling.

[Json.Decode]: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode


## Success

    port action : (Json.Decode.Value -> msg) -> Sub msg

    port alarm : Json.Encode.Value -> Cmd msg


## Failure

    port action : (Int -> msg) -> Sub msg

    port alarm : String -> Cmd msg

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnsafePorts" initialModuleContext
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node context =
    ( [], rememberImportedModule (Node.value node) context )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor nodes context =
    ( List.concatMap (checkDeclaration context) nodes, context )


checkDeclaration : ModuleContext -> Node Declaration -> List (Error {})
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


checkPort : ModuleContext -> String -> Node TypeAnnotation -> Maybe PortType -> List (Error {})
checkPort context name portArguments portReturnType =
    case portReturnType of
        Just IncomingPort ->
            checkIncomingPort context name portArguments

        Just OutgoingPort ->
            checkOutgoingPort context name portArguments

        Nothing ->
            -- There is something off about this port declaration. The compiler will complain about this.
            []


checkIncomingPort : ModuleContext -> String -> Node TypeAnnotation -> List (Error {})
checkIncomingPort context name portArguments =
    case Node.value portArguments of
        TypeAnnotation.FunctionTypeAnnotation subMessageType _ ->
            checkPortArguments context (unsafePortError name) subMessageType

        _ ->
            -- There is something off about this port declaration. The compiler will complain about this
            []


checkOutgoingPort : ModuleContext -> String -> Node TypeAnnotation -> List (Error {})
checkOutgoingPort context name portArguments =
    checkPortArguments context (unsafePortError name) portArguments


checkPortArguments : ModuleContext -> (Node String -> Error {}) -> Node TypeAnnotation -> List (Error {})
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
        , imports : Dict String ModuleName
        }


initialModuleContext : ModuleContext
initialModuleContext =
    Context
        { aliases = Dict.empty
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


unsafePortError : String -> Node String -> Error {}
unsafePortError name portType =
    Rule.error
        { message = "Port `" ++ name ++ "` expects unsafe " ++ Node.value portType ++ " data."
        , details =
            [ "When a port expecting an unsafe type receives data of another type it will cause a runtime error."
            , "You should change this port to use `Json.Decode.Value` and use a `Decoder` result to handle any mismatched type errors."
            ]
        }
        (Node.range portType)


formatType : ( ModuleName, String ) -> String
formatType ( moduleName, name ) =
    "`" ++ String.join "." (moduleName ++ [ name ]) ++ "`"
