module NoUnusedPorts exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnusedPorts" initialModuleContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    case Node.value node of
        Module.PortModule { exposingList } ->
            ( [], rememberExposing exposingList context )

        _ ->
            ( [], context )


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
        ( Rule.OnEnter, Expression.FunctionOrValue _ name ) ->
            ( [], rememberFunctionCall name context )

        _ ->
            ( [], context )


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation { ports, exposed } =
    ports
        |> removePorts exposed
        |> reportUnusedPorts


removePorts : Set String -> Dict String data -> Dict String data
removePorts remove ports =
    Set.foldl Dict.remove ports remove



--- REPORT


reportUnusedPorts : Dict String Range -> List (Error {})
reportUnusedPorts ports =
    Dict.toList ports
        |> List.map reportUnusedPort


reportUnusedPort : ( String, Range ) -> Error {}
reportUnusedPort ( name, range ) =
    Rule.error
        { message = "Port `" ++ name ++ "` is not used anywhere."
        , details = [ "TODO: This is a problem because..." ]
        }
        range



--- CONTEXT


type alias ModuleContext =
    { ports : Dict String Range
    , exposed : Set String
    }


initialModuleContext : ModuleContext
initialModuleContext =
    { ports = Dict.empty
    , exposed = Set.empty
    }


rememberPort : Node String -> ModuleContext -> ModuleContext
rememberPort node context =
    { context | ports = Dict.insert (Node.value node) (Node.range node) context.ports }


rememberExposing : Node Exposing -> ModuleContext -> ModuleContext
rememberExposing node context =
    case Node.value node of
        Exposing.Explicit nodes ->
            rememberExposedList nodes context

        _ ->
            context


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
    { context | exposed = Set.insert name context.exposed }


rememberFunctionCall : String -> ModuleContext -> ModuleContext
rememberFunctionCall name context =
    { context | ports = Dict.remove name context.ports }
