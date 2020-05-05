module NoUnusedPorts exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnusedPorts" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.PortDeclaration { name } ->
            [ reportUnusedPort name ]

        _ ->
            []


reportUnusedPort : Node String -> Error {}
reportUnusedPort node =
    Rule.error
        { message = "Port `" ++ Node.value node ++ "` is not used anywhere."
        , details = [ "TODO: This is a problem because..." ]
        }
        (Node.range node)
