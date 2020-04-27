module NoDuplicatePorts exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Direction, Error, Rule)


{-| Ensure that port names are unique across your project.

Problem: When there are multiple ports with the same name you may encounter a JavaScript runtime error.

You should not enable this rule when the project is an Elm package.

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoDuplicatePorts" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationVisitor declarationVisitor


declarationVisitor : Node Declaration -> Direction -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Declaration.PortDeclaration { name } ) ->
            ( [], Dict.insert (Node.value name) (Node.range name) context )

        _ ->
            ( [], context )


type alias PortLocation =
    ( Rule.ModuleKey, Range )


type alias ProjectContext =
    Dict String (Dict ModuleName PortLocation)


type alias ModuleContext =
    Dict String Range


initialProjectContext : ProjectContext
initialProjectContext =
    Dict.empty


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ _ =
    Dict.empty


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleNameNode =
    Dict.map (fromModuleToProjectPort moduleKey (Node.value moduleNameNode))


fromModuleToProjectPort : Rule.ModuleKey -> ModuleName -> String -> Range -> Dict ModuleName PortLocation
fromModuleToProjectPort moduleKey moduleName _ range =
    Dict.singleton moduleName ( moduleKey, range )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newPorts oldPorts =
    Dict.merge Dict.insert mergePortLocationDicts Dict.insert newPorts oldPorts Dict.empty


mergePortLocationDicts : String -> Dict ModuleName PortLocation -> Dict ModuleName PortLocation -> ProjectContext -> ProjectContext
mergePortLocationDicts portName newLocations oldLocations =
    Dict.insert portName (Dict.union newLocations oldLocations)


finalProjectEvaluation : ProjectContext -> List (Error scope)
finalProjectEvaluation projectContext =
    projectContext
        |> Dict.toList
        |> List.concatMap errorsFromPortLocations


errorsFromPortLocations : ( String, Dict ModuleName PortLocation ) -> List (Error scope)
errorsFromPortLocations ( portName, locations ) =
    if Dict.size locations < 2 then
        []

    else
        locations
            |> Dict.values
            |> List.map (errorFromPortLocation portName)


errorFromPortLocation : String -> ( Rule.ModuleKey, Range ) -> Error scope
errorFromPortLocation portName ( moduleKey, range ) =
    Rule.errorForModule moduleKey (error portName) range


error : String -> { message : String, details : List String }
error portName =
    { message = String.concat [ "Another port named `", portName, "` already exists." ]
    , details = [ "When there are multiple ports with the same name you may encounter a JavaScript runtime error." ]
    }
