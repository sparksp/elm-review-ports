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
        |> Rule.withFinalModuleEvaluation finalModuleEvaluation


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    case Node.value node of
        Module.PortModule { exposingList } ->
            ( [], rememberExposing exposingList context )

        _ ->
            ( [], context )


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node context =
    ( [], rememberImportedModule (Node.value node) context )


declarationVisitor : Node Declaration -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.PortDeclaration { name } ) ->
            ( [], rememberPort name context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
            ( [], rememberFunctionCall moduleName name context )

        _ ->
            ( [], context )


finalModuleEvaluation : ModuleContext -> List (Error {})
finalModuleEvaluation context =
    context
        |> removeExposedPorts
        |> removeCalledLocalPorts
        |> reportUnusedLocalPorts


finalEvaluation : ProjectContext -> List (Error scope)
finalEvaluation context =
    context
        |> removeCalledPorts
        |> reportUnusedPorts



--- REPORT


reportUnusedLocalPorts : ModuleContext -> List (Error {})
reportUnusedLocalPorts { ports } =
    Dict.toList ports
        |> List.map reportUnusedLocalPort


reportUnusedLocalPort : ( String, Range ) -> Error {}
reportUnusedLocalPort ( portName, range ) =
    Rule.error (report portName) range


reportUnusedPorts : ProjectContext -> List (Error scope)
reportUnusedPorts { ports } =
    Dict.toList ports
        |> List.map reportUnusedPort


reportUnusedPort : ( ( ModuleName, String ), ( Rule.ModuleKey, Range ) ) -> Error scope
reportUnusedPort ( ( _, portName ), ( moduleKey, range ) ) =
    Rule.errorForModule moduleKey (report portName) range


report : String -> { message : String, details : List String }
report portName =
    { message = "Port `" ++ portName ++ "` is not used anywhere."
    , details = [ "TODO: This is a problem because..." ]
    }



--- CONTEXT


type Exposed
    = ExposedAll
    | ExposedList (Set String)


type alias ProjectContext =
    { ports : ProjectPorts
    , functionCalls : Set ( ModuleName, String )
    }


type alias ProjectPorts =
    Dict ( ModuleName, String ) ( Rule.ModuleKey, Range )


initialProjectContext : ProjectContext
initialProjectContext =
    { ports = Dict.empty
    , functionCalls = Set.empty
    }


type alias ModuleContext =
    { exposed : Exposed
    , functionCalls : Set ( ModuleName, String )
    , importedAliases : Dict ModuleName ModuleName
    , importedFunctions : Dict ( ModuleName, String ) ( ModuleName, String )
    , projectPorts : Set ( ModuleName, String )
    , ports : Dict String Range
    }


initialModuleContext : ModuleContext
initialModuleContext =
    { exposed = ExposedList Set.empty
    , functionCalls = Set.empty
    , importedAliases = Dict.empty
    , importedFunctions = Dict.empty
    , projectPorts = Set.empty
    , ports = Dict.empty
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ context =
    { initialModuleContext
        | projectPorts = context.ports |> Dict.keys |> Set.fromList
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName context =
    { ports =
        context
            |> removeLocalPorts
            |> .ports
            |> Dict.foldl (fromModuleToProjectPort moduleKey (Node.value moduleName)) Dict.empty
    , functionCalls = context.functionCalls
    }


fromModuleToProjectPort : Rule.ModuleKey -> ModuleName -> String -> Range -> ProjectPorts -> ProjectPorts
fromModuleToProjectPort moduleKey moduleName portName portRange dict =
    Dict.insert ( moduleName, portName ) ( moduleKey, portRange ) dict


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new old =
    { ports = Dict.foldl Dict.insert old.ports new.ports
    , functionCalls = Set.union old.functionCalls new.functionCalls
    }


rememberExposing : Node Exposing -> ModuleContext -> ModuleContext
rememberExposing node context =
    case Node.value node of
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
        |> rememberImportedFunctionSet (filterByModuleName moduleName context.projectPorts)


filterByModuleName : ModuleName -> Set ( ModuleName, String ) -> Set ( ModuleName, String )
filterByModuleName moduleName set =
    set |> Set.filter (\( name, _ ) -> name == moduleName)


rememberImportedFunctionSet : Set ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImportedFunctionSet set context =
    Set.foldl rememberImportedFunction context set


rememberImportedFunction : ( ModuleName, String ) -> ModuleContext -> ModuleContext
rememberImportedFunction ( moduleName, name ) context =
    { context | importedFunctions = Dict.insert ( [], name ) ( moduleName, name ) context.importedFunctions }


rememberPort : Node String -> ModuleContext -> ModuleContext
rememberPort node context =
    { context | ports = Dict.insert (Node.value node) (Node.range node) context.ports }


rememberFunctionCall : ModuleName -> String -> ModuleContext -> ModuleContext
rememberFunctionCall moduleName name context =
    let
        functionCall =
            expandFunctionCall context ( moduleName, name )
    in
    { context | functionCalls = Set.insert functionCall context.functionCalls }


removeExposedPorts : ModuleContext -> ModuleContext
removeExposedPorts context =
    case context.exposed of
        ExposedAll ->
            { context | ports = Dict.empty }

        ExposedList list ->
            { context | ports = Set.foldl Dict.remove context.ports list }


removeLocalPorts : ModuleContext -> ModuleContext
removeLocalPorts context =
    case context.exposed of
        ExposedAll ->
            context

        ExposedList list ->
            let
                portNames =
                    context.ports |> Dict.keys |> Set.fromList

                localPortNames =
                    Set.diff portNames list
            in
            { context
                | ports = Set.foldl Dict.remove context.ports localPortNames
            }


getLocalFunctions : Set ( ModuleName, String ) -> Set String
getLocalFunctions functionCalls =
    functionCalls
        |> Set.filter isExternalFunction
        |> Set.map Tuple.second


isExternalFunction : ( ModuleName, String ) -> Bool
isExternalFunction ( moduleName, _ ) =
    moduleName == []


removeCalledLocalPorts : ModuleContext -> ModuleContext
removeCalledLocalPorts ({ functionCalls, ports } as context) =
    { context
        | ports = functionCalls |> getLocalFunctions |> Set.foldl Dict.remove ports
    }


removeCalledPorts : ProjectContext -> ProjectContext
removeCalledPorts context =
    { context | ports = Set.foldl Dict.remove context.ports context.functionCalls }


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
