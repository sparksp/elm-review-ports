module NoUnusedPorts exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnusedPorts" initialModuleContext
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias ModuleContext =
    Dict String Range


initialModuleContext : ModuleContext
initialModuleContext =
    Dict.empty


rememberPort : Node String -> ModuleContext -> ModuleContext
rememberPort node context =
    Dict.insert (Node.value node) (Node.range node) context


rememberFunctionCall : String -> ModuleContext -> ModuleContext
rememberFunctionCall name context =
    Dict.remove name context


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
finalEvaluation context =
    context
        |> Dict.toList
        |> List.map reportUnusedPort


reportUnusedPort : ( String, Range ) -> Error {}
reportUnusedPort ( name, range ) =
    Rule.error
        { message = "Port `" ++ name ++ "` is not used anywhere."
        , details = [ "TODO: This is a problem because..." ]
        }
        range
