module NoUnusedPorts exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of ports that are never used in your project.

    config : List Rule
    config =
        [ NoUnusedPorts.rule
        ]

This rule reports any ports that are not used _anywhere in the project_. A port is only considered used if it can be traced to a `main` function.


## Why is this a problem?

Elm is very good at eliminating dead code from the compiled JavaScript. When a port is unused it will not be present in the compiled JavaScript, and when no ports are used the `app.ports` object will be `undefined`. This can lead to JavaScript runtime errors that could take you some time to figure out.

```javascript
var app = Elm.Main.init({
    node: document.getElementById('elm')
});

app.ports.myPort // undefined
```


## When (not) to use this rule

Ports are not allowed in Elm packages - you should not enable this when developing an Elm package.


## Failure

    port module Main exposing (main)

    import Html
    import Json.Encode as E


    -- Port `action` is never used.
    port action : (E.Value -> msg) -> Sub msg

    port alarm : E.Value -> msg

    -- Port `alarm` is never used, because `play` is never used.
    play : Cmd msg
    play =
        alarm (E.string "play")

    main =
        Html.text "Hello"

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnusedPorts" initialProjectContext
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema



--- VISITORS


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (guardedDeclarationVisitor importVisitor)
        |> Rule.withDeclarationListVisitor (guardedDeclarationVisitor declarationListVisitor)
        |> Rule.withDeclarationVisitor (guardedExpressionVisitor declarationVisitor)
        |> Rule.withExpressionVisitor (guardedExpressionVisitor expressionVisitor)


{-| Only visit imports and declaration lists if are a port module or know about any ports.

  - We may be passed ports from an import, or
  - We may declare our own ports.

-}
guardedDeclarationVisitor : (a -> ModuleContext -> ( List (Error {}), ModuleContext )) -> a -> ModuleContext -> ( List (Error {}), ModuleContext )
guardedDeclarationVisitor visitor a context =
    if context.isPortModule || context.hasPorts then
        visitor a context

    else
        ( [], context )


{-| Only visit declarations and expressions if we know about any ports.

  - We may be passed ports from an import, or
  - We may have declared our own ports.

-}
guardedExpressionVisitor : (a -> b -> ModuleContext -> ( List (Error {}), ModuleContext )) -> a -> b -> ModuleContext -> ( List (Error {}), ModuleContext )
guardedExpressionVisitor visitor a b context =
    if context.hasPorts then
        visitor a b context

    else
        ( [], context )


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    case Node.value node of
        Module.PortModule _ ->
            ( [], { context | isPortModule = True } )

        _ ->
            ( [], { context | isPortModule = False } )


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node context =
    ( [], rememberImportedModule (Node.value node) context )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor declarations context =
    ( [], List.foldl rememberDeclaration context declarations )


declarationVisitor : Node Declaration -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.FunctionDeclaration { declaration } ) ->
            let
                name : String
                name =
                    declaration |> Node.value |> .name |> Node.value
            in
            ( [], rememberCurrentFunction ( context.moduleName, name ) context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
            ( [], rememberFunctionCall ( moduleName, name ) context )

        _ ->
            ( [], context )


finalEvaluation : ProjectContext -> List (Error scope)
finalEvaluation { functionCalls, ports } =
    ports |> Dict.toList |> List.map (reportUnusedPort functionCalls)



--- REPORT


reportUnusedPort : FunctionCalls -> ( ( ModuleName, String ), Port ) -> Error scope
reportUnusedPort functionCalls ( ( moduleName, portName ), Port { range, moduleKey } ) =
    let
        callers : Maybe (Set ( ModuleName, String ))
        callers =
            Dict.get ( moduleName, portName ) functionCalls
    in
    Rule.errorForModule moduleKey (report portName callers) range


report : String -> Maybe (Set ( ModuleName, String )) -> { message : String, details : List String }
report portName callers =
    { message = "Port `" ++ portName ++ "` is never used (Warning: can cause JS runtime errors)"
    , details =
        "Unused ports are not available in the compiled JavaScript and can cause runtime errors when you try to access them."
            :: "You should either use this port somewhere, or remove it at the location I pointed at. This may highlight some other unused code in your project too."
            :: "Warning: If you remove this port, remember to remove any calls to it in your JavaScript code too."
            :: callerDetails callers
    }


callerDetails : Maybe (Set ( ModuleName, String )) -> List String
callerDetails maybeCallers =
    case maybeCallers of
        Nothing ->
            []

        Just callers ->
            [ "I found this port called by the following functions, but none of them trace back to a `main` function:"
            , callers |> Set.toList |> List.map formatCaller |> String.join "\n"
            ]


formatCaller : ( ModuleName, String ) -> String
formatCaller ( moduleName, caller ) =
    "-> " ++ String.join "." moduleName ++ "." ++ caller



--- CONTEXT


type Port
    = Port { range : Range, moduleKey : Rule.ModuleKey }


type alias ProjectContext =
    { functionCalls : FunctionCalls
    , ports : ProjectPorts
    , usedPorts : ProjectPorts
    }


type alias FunctionCalls =
    Dict ( ModuleName, String ) (Set ( ModuleName, String ))


type alias ProjectPorts =
    Dict ( ModuleName, String ) Port


initialProjectContext : ProjectContext
initialProjectContext =
    { functionCalls = Dict.empty
    , ports = Dict.empty
    , usedPorts = Dict.empty
    }


type alias ModuleContext =
    { currentFunction : ( ModuleName, String )
    , functionCalls : FunctionCalls
    , hasPorts : Bool
    , importedAliases : Dict ModuleName ModuleName
    , importedFunctions : Dict String ModuleName
    , isPortModule : Bool
    , moduleKey : Rule.ModuleKey
    , moduleName : ModuleName
    , ports : ProjectPorts
    }


initialModuleContext : { functionCalls : FunctionCalls, moduleKey : Rule.ModuleKey, moduleName : ModuleName, ports : ProjectPorts } -> ModuleContext
initialModuleContext { functionCalls, moduleKey, moduleName, ports } =
    { currentFunction = ( [], "" )
    , functionCalls = functionCalls
    , hasPorts = not (Dict.isEmpty ports)
    , importedAliases = Dict.empty
    , importedFunctions = Dict.empty
    , isPortModule = False
    , moduleKey = moduleKey
    , moduleName = moduleName
    , ports = ports
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule moduleKey moduleName context =
    initialModuleContext
        { functionCalls = context.functionCalls
        , moduleKey = moduleKey
        , moduleName = Node.value moduleName
        , ports = context.ports
        }


type alias SearchData =
    { calls : Set ( ModuleName, String )
    , functionCalls : FunctionCalls
    , main : ( ModuleName, String )
    }


type alias SearchContext =
    { calls : FunctionCalls
    , used : Set ( ModuleName, String )
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject _ _ { functionCalls, moduleName, ports } =
    let
        finder =
            findUsedPort
                { calls = Set.empty
                , functionCalls = functionCalls
                , main = ( moduleName, "main" )
                }

        { used, calls } =
            Dict.foldl finder (SearchContext Dict.empty Set.empty) ports

        ( usedPorts, unusedPorts ) =
            Dict.partition (\name _ -> Set.member name used) ports
    in
    { functionCalls = calls
    , ports = unusedPorts
    , usedPorts = usedPorts
    }


findUsedPort : SearchData -> ( ModuleName, String ) -> Port -> SearchContext -> SearchContext
findUsedPort data portName _ context =
    findUsedFunction portName data portName context


findUsedFunction : ( ModuleName, String ) -> SearchData -> ( ModuleName, String ) -> SearchContext -> SearchContext
findUsedFunction portName ({ calls, functionCalls, main } as data) function ({ used } as context) =
    if function == main then
        -- Debug.log "stop: found main" <|
        { context | used = Set.union calls used }

    else if Set.member function used then
        -- Debug.log "stop: found a used function" <|
        { context | used = Set.union calls used }

    else if Set.member function calls then
        -- Debug.log "stop: recursion" <|
        { context | calls = updateCalls portName function context.calls }

    else
        case Dict.get function functionCalls of
            Nothing ->
                -- Debug.log "stop: function unused" <|
                { context | calls = updateCalls portName function context.calls }

            Just callers ->
                Set.foldl (findUsedFunction portName { data | calls = Set.insert function calls }) context callers


updateCalls : ( ModuleName, String ) -> ( ModuleName, String ) -> FunctionCalls -> FunctionCalls
updateCalls portName function functionCalls =
    if portName == function then
        functionCalls

    else
        Dict.update portName (insertOrSetFunctionCall function) functionCalls


insertOrSetFunctionCall : ( ModuleName, String ) -> Maybe (Set ( ModuleName, String )) -> Maybe (Set ( ModuleName, String ))
insertOrSetFunctionCall item maybeSet =
    case maybeSet of
        Nothing ->
            Just (Set.singleton item)

        Just set ->
            Just (Set.insert item set)


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts a b =
    let
        usedPorts =
            mergePorts a.usedPorts b.usedPorts
    in
    { functionCalls = mergeFunctionCalls a.functionCalls b.functionCalls
    , ports = mergePorts a.ports b.ports |> removePorts usedPorts
    , usedPorts = usedPorts
    }


mergeFunctionCalls : FunctionCalls -> FunctionCalls -> FunctionCalls
mergeFunctionCalls a b =
    Dict.merge Dict.insert insertUnion Dict.insert a b Dict.empty


insertUnion : ( ModuleName, String ) -> Set ( ModuleName, String ) -> Set ( ModuleName, String ) -> FunctionCalls -> FunctionCalls
insertUnion function a b =
    Dict.insert function (Set.union a b)


mergePorts : ProjectPorts -> ProjectPorts -> ProjectPorts
mergePorts a b =
    Dict.foldl Dict.insert a b


removePorts : ProjectPorts -> ProjectPorts -> ProjectPorts
removePorts remove ports =
    Dict.diff ports remove


rememberImportedModule : Import -> ModuleContext -> ModuleContext
rememberImportedModule { moduleName, moduleAlias, exposingList } context =
    context
        |> rememberImportedAlias (Node.value moduleName) moduleAlias
        |> rememberImportedList (Node.value moduleName) exposingList


rememberImportedAlias : ModuleName -> Maybe (Node ModuleName) -> ModuleContext -> ModuleContext
rememberImportedAlias moduleName maybeModuleAlias context =
    case Maybe.map Node.value maybeModuleAlias of
        Just moduleAlias ->
            { context | importedAliases = Dict.insert moduleAlias moduleName context.importedAliases }

        Nothing ->
            context


rememberImportedList : ModuleName -> Maybe (Node Exposing) -> ModuleContext -> ModuleContext
rememberImportedList moduleName exposingList context =
    case Maybe.map Node.value exposingList of
        Just (Exposing.Explicit list) ->
            rememberImportedExplicitList moduleName list context

        Just (Exposing.All _) ->
            rememberImportedAll moduleName context

        Nothing ->
            context


rememberImportedExplicitList : ModuleName -> List (Node Exposing.TopLevelExpose) -> ModuleContext -> ModuleContext
rememberImportedExplicitList moduleName list context =
    List.foldl (rememberImportedItem moduleName) context list


rememberImportedItem : ModuleName -> Node Exposing.TopLevelExpose -> ModuleContext -> ModuleContext
rememberImportedItem moduleName item context =
    case Node.value item of
        Exposing.FunctionExpose name ->
            rememberImportedFunction ( moduleName, name ) context

        _ ->
            context


rememberImportedAll : ModuleName -> ModuleContext -> ModuleContext
rememberImportedAll moduleName context =
    context
        |> rememberImportedFunctionList (filterByFirst moduleName (Dict.keys context.ports))


rememberImportedFunctionList : List ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImportedFunctionList functions context =
    functions |> List.foldl rememberImportedFunction context


rememberImportedFunction : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImportedFunction ( moduleName, name ) context =
    { context | importedFunctions = Dict.insert name moduleName context.importedFunctions }


rememberDeclaration : Node Declaration -> ModuleContext -> ModuleContext
rememberDeclaration node context =
    case Node.value node of
        Declaration.PortDeclaration { name } ->
            rememberPort name context

        _ ->
            context


rememberPort : Node String -> ModuleContext -> ModuleContext
rememberPort node context =
    let
        portName =
            ( context.moduleName, Node.value node )
    in
    { context
        | hasPorts = True
        , ports = Dict.insert portName (Port { range = Node.range node, moduleKey = context.moduleKey }) context.ports
    }


rememberCurrentFunction : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberCurrentFunction function context =
    { context | currentFunction = function }


rememberFunctionCall : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberFunctionCall function context =
    let
        functionCall =
            expandFunctionCall context function
    in
    { context | functionCalls = Dict.update functionCall (maybeSetInsert context.currentFunction) context.functionCalls }


expandFunctionCall : ModuleContext -> ( ModuleName, String ) -> ( ModuleName, String )
expandFunctionCall { importedAliases, importedFunctions, moduleName } ( moduleCall, functionCall ) =
    let
        expandedModule : ModuleName
        expandedModule =
            case moduleCall of
                [] ->
                    lookupFunctionModule importedFunctions moduleName functionCall

                _ ->
                    lookupModuleAlias importedAliases moduleCall
    in
    ( expandedModule, functionCall )


lookupFunctionModule : Dict String ModuleName -> ModuleName -> String -> ModuleName
lookupFunctionModule importedFunctions defaultModuleName function =
    Dict.get function importedFunctions
        |> Maybe.withDefault defaultModuleName


lookupModuleAlias : Dict ModuleName ModuleName -> ModuleName -> ModuleName
lookupModuleAlias importedAliases moduleName =
    Dict.get moduleName importedAliases
        |> Maybe.withDefault moduleName


filterByFirst : a -> List ( a, b ) -> List ( a, b )
filterByFirst first tuples =
    tuples |> List.filter (\( value, _ ) -> value == first)


maybeSetInsert : comparable -> Maybe (Set comparable) -> Maybe (Set comparable)
maybeSetInsert item maybeSet =
    case maybeSet of
        Nothing ->
            Just (Set.singleton item)

        Just set ->
            Just (Set.insert item set)
