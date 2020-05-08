module NoUnusedPorts exposing (rule)

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


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    ( [], rememberExposing (node |> Node.value |> Module.exposingList) context )


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node context =
    ( [], rememberImportedModule (Node.value node) context )


declarationVisitor : Node Declaration -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.PortDeclaration { name } ) ->
            ( [], rememberPort name context )

        ( Rule.OnEnter, Declaration.FunctionDeclaration { declaration } ) ->
            let
                name : String
                name =
                    declaration |> Node.value |> .name |> Node.value
            in
            ( [], rememberCurrentFunction ( context.moduleName, name ) context )

        ( Rule.OnExit, Declaration.FunctionDeclaration _ ) ->
            ( [], { context | currentFunction = Nothing } )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
            ( [], rememberFunctionCall moduleName name context )

        _ ->
            ( [], context )


finalEvaluation : ProjectContext -> List (Error scope)
finalEvaluation context =
    context
        |> reportUnusedPorts



--- REPORT


reportUnusedPorts : ProjectContext -> List (Error scope)
reportUnusedPorts { ports } =
    ports
        |> Dict.toList
        |> List.map reportUnusedPort


reportUnusedPort : ( ( ModuleName, String ), Port ) -> Error scope
reportUnusedPort ( ( _, portName ), Port range moduleKey ) =
    Rule.errorForModule moduleKey (report portName) range


report : String -> { message : String, details : List String }
report portName =
    { message = "Port `" ++ portName ++ "` is not used anywhere."
    , details = [ "TODO: This is a problem because..." ]
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
foldProjectContexts new old =
    { functionCalls = Dict.merge Dict.insert mergeFunctionCalls Dict.insert old.functionCalls new.functionCalls Dict.empty
    , ports = Dict.foldl Dict.insert new.ports old.ports
    , usedPorts = Dict.foldl Dict.insert new.usedPorts old.usedPorts
    }
        |> removeUsedPorts


removeUsedPorts : ProjectContext -> ProjectContext
removeUsedPorts context =
    { context
        | ports = Dict.foldl (\k _ -> Dict.remove k) context.ports context.usedPorts
    }


mergeFunctionCalls : ( ModuleName, String ) -> Set ( ModuleName, String ) -> Set ( ModuleName, String ) -> FunctionCalls -> FunctionCalls
mergeFunctionCalls function scopeA scopeB =
    Dict.insert function (Set.union scopeA scopeB)


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
        |> rememberImportedFunctionSet (filterByModuleName moduleName (Dict.keys context.ports))


filterByModuleName : ModuleName -> List ( ModuleName, String ) -> List ( ModuleName, String )
filterByModuleName moduleName functions =
    functions |> List.filter (\( name, _ ) -> name == moduleName)


rememberImportedFunctionSet : List ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImportedFunctionSet functions context =
    functions |> List.foldl rememberImportedFunction context


rememberImportedFunction : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImportedFunction ( moduleName, name ) context =
    { context | importedFunctions = Dict.insert ( [], name ) ( moduleName, name ) context.importedFunctions }


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
        |> rememberImportedFunction function


rememberFunctionCall : ModuleName -> String -> ModuleContext -> ModuleContext
rememberFunctionCall moduleName name context =
    let
        functionCall =
            expandFunctionCall context ( moduleName, name )
    in
    { context | functionCalls = Dict.update functionCall (updateFunctionCall context.currentFunction) context.functionCalls }


updateFunctionCall : Maybe comparable -> Maybe (Set comparable) -> Maybe (Set comparable)
updateFunctionCall maybeParent maybeParents =
    let
        parents : Set comparable
        parents =
            maybeParents |> Maybe.withDefault Set.empty
    in
    case maybeParent of
        Nothing ->
            Just parents

        Just parent ->
            Just (Set.insert parent parents)


isPortUsed : ModuleContext -> ( ModuleName, String ) -> Bool
isPortUsed context name =
    case Dict.get name context.functionCalls of
        Nothing ->
            False

        Just callers ->
            callers |> Set.toList |> List.any (isFunctionCalledViaMain context)


isFunctionCalledViaMain : ModuleContext -> ( ModuleName, String ) -> Bool
isFunctionCalledViaMain context ( moduleName, name ) =
    if ( moduleName, name ) == ( context.moduleName, "main" ) then
        isFunctionExposed context.exposed name

    else
        case Dict.get ( moduleName, name ) context.functionCalls of
            Nothing ->
                False

            Just callers ->
                callers |> Set.toList |> List.any (isFunctionCalledViaMain context)


isFunctionExposed : Exposed -> String -> Bool
isFunctionExposed exposed name =
    case exposed of
        ExposedAll ->
            True

        ExposedList list ->
            Set.member name list


expandFunctionCall : ModuleContext -> ( ModuleName, String ) -> ( ModuleName, String )
expandFunctionCall context ( moduleName, function ) =
    Dict.get ( moduleName, function ) context.importedFunctions
        |> Maybe.withDefault ( moduleName, function )
        |> expandModuleName context


expandModuleName : ModuleContext -> ( ModuleName, String ) -> ( ModuleName, String )
expandModuleName context ( moduleName, function ) =
    ( Dict.get moduleName context.importedAliases
        |> Maybe.withDefault moduleName
    , function
    )
