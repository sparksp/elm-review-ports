module NoDuplicatePorts exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)


{-| Forbid duplicate port names in your project.

    config : List Rule
    config =
        [ NoDuplicatePorts.rule
        ]


## Why is this a problem?

The only way to tell which port you want to address in JavaScript is by its name, and so these must be unique within your project. When there are multiple ports with the same name you may encounter a JavaScript runtime error.

It is common practice to have a single `Ports` module to contain all of the ports in a project. The `Ports` module can then be imported anywhere that needs access to a port.


## When (not) to use this rule

Ports are not allowed in Elm packages - you should not enable this when developing an Elm package.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template sparksp/elm-review-ports/example --rules NoDuplicatePorts
```

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



--- INTERNALS


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor nodes context =
    ( [], List.foldl rememberPortDeclaration context nodes )


rememberPortDeclaration : Node Declaration -> ModuleContext -> ModuleContext
rememberPortDeclaration node context =
    case Node.value node of
        Declaration.PortDeclaration { name } ->
            Dict.insert (Node.value name) (Node.range name) context

        _ ->
            context


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


finalProjectEvaluation : ProjectContext -> List (Rule.Error scope)
finalProjectEvaluation projectContext =
    projectContext
        |> Dict.toList
        |> fastConcatMap errorsFromPortLocations


errorsFromPortLocations : ( String, Dict ModuleName PortLocation ) -> List (Rule.Error scope)
errorsFromPortLocations ( portName, locations ) =
    if Dict.size locations < 2 then
        []

    else
        locations
            |> Dict.values
            |> List.map (errorFromPortLocation portName)


errorFromPortLocation : String -> ( Rule.ModuleKey, Range ) -> Rule.Error scope
errorFromPortLocation portName ( moduleKey, range ) =
    Rule.errorForModule moduleKey (error portName) range


error : String -> { message : String, details : List String }
error portName =
    { message = String.concat [ "Another port named `", portName, "` already exists." ]
    , details = [ "When there are multiple ports with the same name you may encounter a JavaScript runtime error." ]
    }



--- FASTER LIST OPERATIONS


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn list =
    List.foldr (fn >> (++)) [] list
