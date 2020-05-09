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

This rule reports any ports that are not used _anywhere in the project_. A port is only considered used if it can be traced to an exposed `main` function.


## Why is this a problem?

Elm is very good at elimiating dead code from the compiled JavaScript. When a port is unused it will not be present in the compiled JavaScript, and when no ports are used the `app.ports` object will be `undefined`. This may lead to JavaScript runtime errors that could take you some time to figure out.

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


    -- Port `action` is never used.
    port action : (String -> msg) -> Sub msg

    port alarm : String -> msg

    -- Port `alarm` is never used, because `play` is never used.
    play : Cmd msg
    play =
        alarm "play"

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
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    ( [], rememberExposing (node |> Node.value |> Module.exposingList) context )


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

        ( Rule.OnExit, Declaration.FunctionDeclaration _ ) ->
            ( [], forgetCurrentFunction context )

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
finalEvaluation { ports } =
    ports |> reportUnusedPorts



--- REPORT


reportUnusedPorts : ProjectPorts -> List (Error scope)
reportUnusedPorts ports =
    ports |> Dict.toList |> List.map reportUnusedPort


reportUnusedPort : ( ( ModuleName, String ), Port ) -> Error scope
reportUnusedPort ( ( _, portName ), Port range moduleKey ) =
    Rule.errorForModule moduleKey (report portName) range


report : String -> { message : String, details : List String }
report portName =
    { message = "Port `" ++ portName ++ "` is never used."
    , details =
        [ "This port is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project."
        , "Unused ports are not available in the compiled JavaScript and may cause runtime errors if you try to access them."
        ]
    }



--- CONTEXT


type Port
    = Port Range Rule.ModuleKey


type Exposed
    = ExposedAll
    | ExposedList (Set String)


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
    { currentFunction : Maybe ( ModuleName, String )
    , exposed : Exposed
    , functionCalls : FunctionCalls
    , importedAliases : Dict ModuleName ModuleName
    , importedFunctions : Dict ( ModuleName, String ) ( ModuleName, String )
    , moduleKey : Rule.ModuleKey
    , moduleName : ModuleName
    , ports : ProjectPorts
    }


initialModuleContext : { functionCalls : FunctionCalls, moduleKey : Rule.ModuleKey, moduleName : ModuleName, ports : ProjectPorts } -> ModuleContext
initialModuleContext { functionCalls, moduleKey, moduleName, ports } =
    { currentFunction = Nothing
    , exposed = ExposedList Set.empty
    , functionCalls = functionCalls
    , importedAliases = Dict.empty
    , importedFunctions = Dict.empty
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


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject _ _ context =
    let
        ( used, unused ) =
            Dict.partition (\name _ -> isPortUsed context name) context.ports
    in
    { functionCalls = context.functionCalls
    , ports = unused
    , usedPorts = used
    }


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


rememberExposing : Exposing -> ModuleContext -> ModuleContext
rememberExposing exposing_ context =
    case exposing_ of
        Exposing.Explicit nodes ->
            rememberExposedList nodes context

        Exposing.All _ ->
            rememberExposingAll context


rememberExposingAll : ModuleContext -> ModuleContext
rememberExposingAll context =
    { context | exposed = ExposedAll }


rememberExposedList : List (Node Exposing.TopLevelExpose) -> ModuleContext -> ModuleContext
rememberExposedList nodes context =
    List.foldl rememberExposedItem context nodes


rememberExposedItem : Node Exposing.TopLevelExpose -> ModuleContext -> ModuleContext
rememberExposedItem node context =
    case Node.value node of
        Exposing.FunctionExpose name ->
            rememberExposedFunction name context

        _ ->
            context


rememberExposedFunction : String -> ModuleContext -> ModuleContext
rememberExposedFunction name context =
    case context.exposed of
        ExposedAll ->
            context

        ExposedList list ->
            { context | exposed = ExposedList (Set.insert name list) }


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

        _ ->
            context


rememberImportedList : ModuleName -> Maybe (Node Exposing) -> ModuleContext -> ModuleContext
rememberImportedList moduleName exposingList context =
    case Maybe.map Node.value exposingList of
        Just (Exposing.Explicit list) ->
            rememberImportedExplicitList moduleName list context

        Just (Exposing.All _) ->
            rememberImportedAll moduleName context

        _ ->
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
    { context | importedFunctions = Dict.insert ( [], name ) ( moduleName, name ) context.importedFunctions }


rememberDeclaration : Node Declaration -> ModuleContext -> ModuleContext
rememberDeclaration node context =
    case Node.value node of
        Declaration.PortDeclaration { name } ->
            rememberPort name context

        Declaration.FunctionDeclaration { declaration } ->
            let
                name : String
                name =
                    declaration |> Node.value |> .name |> Node.value
            in
            rememberImportedFunction ( context.moduleName, name ) context

        _ ->
            context


rememberPort : Node String -> ModuleContext -> ModuleContext
rememberPort node context =
    let
        portName =
            ( context.moduleName, Node.value node )
    in
    { context | ports = Dict.insert portName (Port (Node.range node) context.moduleKey) context.ports }
        |> rememberImportedFunction portName


rememberCurrentFunction : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberCurrentFunction function context =
    { context | currentFunction = Just function }


forgetCurrentFunction : ModuleContext -> ModuleContext
forgetCurrentFunction context =
    { context | currentFunction = Nothing }


rememberFunctionCall : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberFunctionCall function context =
    let
        functionCall =
            expandFunctionCall context function
    in
    { context | functionCalls = Dict.update functionCall (maybeSetInsert context.currentFunction) context.functionCalls }


isPortUsed : ModuleContext -> ( ModuleName, String ) -> Bool
isPortUsed context name =
    case Dict.get name context.functionCalls of
        Nothing ->
            False

        Just callers ->
            setAny (isFunctionCalledViaMain context) callers


isFunctionCalledViaMain : ModuleContext -> ( ModuleName, String ) -> Bool
isFunctionCalledViaMain context ( moduleName, name ) =
    if ( moduleName, name ) == ( context.moduleName, "main" ) then
        isFunctionExposed context.exposed name

    else
        case Dict.get ( moduleName, name ) context.functionCalls of
            Nothing ->
                False

            Just callers ->
                setAny (isFunctionCalledViaMain context) callers


isFunctionExposed : Exposed -> String -> Bool
isFunctionExposed exposed name =
    case exposed of
        ExposedAll ->
            True

        ExposedList list ->
            Set.member name list


expandFunctionCall : ModuleContext -> ( ModuleName, String ) -> ( ModuleName, String )
expandFunctionCall { importedFunctions, importedAliases } ( moduleName, function ) =
    Dict.get ( moduleName, function ) importedFunctions
        |> Maybe.withDefault ( moduleName, function )
        |> expandModuleName importedAliases


expandModuleName : Dict ModuleName ModuleName -> ( ModuleName, String ) -> ( ModuleName, String )
expandModuleName importedAliases ( moduleName, function ) =
    ( Dict.get moduleName importedAliases
        |> Maybe.withDefault moduleName
    , function
    )


filterByFirst : a -> List ( a, b ) -> List ( a, b )
filterByFirst first tuples =
    tuples |> List.filter (\( value, _ ) -> value == first)


maybeSetInsert : Maybe comparable -> Maybe (Set comparable) -> Maybe (Set comparable)
maybeSetInsert maybeItem maybeSet =
    case ( maybeItem, maybeSet ) of
        ( Nothing, _ ) ->
            maybeSet

        ( Just item, Nothing ) ->
            Just (Set.singleton item)

        ( Just item, Just set ) ->
            Just (Set.insert item set)


setAny : (t -> Bool) -> Set t -> Bool
setAny comp set =
    set |> Set.toList |> List.any comp
