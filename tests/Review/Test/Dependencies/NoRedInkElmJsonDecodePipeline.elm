module Review.Test.Dependencies.NoRedInkElmJsonDecodePipeline exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type exposing (Type(..))
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create "NoRedInk/elm-json-decode-pipeline"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ unsafeModuleName "Json.Decode.Pipeline" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "NoRedInk/elm-json-decode-pipeline"
        , summary = "Use pipelines to build JSON Decoders."
        , deps =
            [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" )
            , ( unsafePackageName "elm/json", unsafeConstraint "1.0.0 <= v < 2.0.0" )
            ]
        , testDeps = []
        , version = Elm.Version.fromString "1.0.0" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Json.Decode.Pipeline"
      , comment = """


# Json.Decode.Pipeline

Use the `(|>)` operator to build JSON decoders.


## Decoding fields

@docs required, requiredAt, optional, optionalAt, hardcoded, custom


## Ending pipelines

@docs resolve

"""
      , aliases = []
      , unions = []
      , binops = []
      , values =
            [ { name = "custom"
              , comment = """ Run the given decoder and feed its result into the pipeline at this point.

Consider this example.

    import Json.Decode as Decode exposing (Decoder, at, int, string)
    import Json.Decode.Pipeline exposing (custom, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> custom (at [ "profile", "name" ] string)
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            \"\"\"
          {
            "id": 123,
            "email": "sam@example.com",
            "profile": {"name": "Sam"}
          }
        \"\"\"


    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }

"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Lambda (Var "a") (Var "b") ]) (Type "Json.Decode.Decoder" [ Var "b" ]))
              }
            , { name = "hardcoded"
              , comment = """ Rather than decoding anything, use a fixed value for the next step in the
pipeline. `harcoded` does not look at the JSON at all.

    import Json.Decode as Decode exposing (Decoder, int, string)
    import Json.Decode.Pipeline exposing (required)

    type alias User =
        { id : Int
        , email : String
        , followers : Int
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> required "email" string
            |> hardcoded 0

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            \"\"\"
          {"id": 123, "email": "sam@example.com"}
        \"\"\"


    -- Ok { id = 123, email = "sam@example.com", followers = 0 }

"""
              , tipe = Lambda (Var "a") (Lambda (Type "Json.Decode.Decoder" [ Lambda (Var "a") (Var "b") ]) (Type "Json.Decode.Decoder" [ Var "b" ]))
              }
            , { name = "optional"
              , comment = """ Decode a field that may be missing or have a null value. If the field is
missing, then it decodes as the `fallback` value. If the field is present,
then `valDecoder` is used to decode its value. If `valDecoder` fails on a
`null` value, then the `fallback` is used as if the field were missing
entirely.

    import Json.Decode as Decode exposing (Decoder, int, null, oneOf, string)
    import Json.Decode.Pipeline exposing (optional, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> optional "name" string "blah"
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            \"\"\"
          {"id": 123, "email": "sam@example.com" }
        \"\"\"


    -- Ok { id = 123, name = "blah", email = "sam@example.com" }

Because `valDecoder` is given an opportunity to decode `null` values before
resorting to the `fallback`, you can distinguish between missing and `null`
values if you need to:

    userDecoder2 =
        Decode.succeed User
            |> required "id" int
            |> optional "name" (oneOf [ string, null "NULL" ]) "MISSING"
            |> required "email" string

"""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Var "a") (Lambda (Type "Json.Decode.Decoder" [ Lambda (Var "a") (Var "b") ]) (Type "Json.Decode.Decoder" [ Var "b" ]))))
              }
            , { name = "optionalAt"
              , comment = """ Decode an optional nested field.
"""
              , tipe = Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Var "a") (Lambda (Type "Json.Decode.Decoder" [ Lambda (Var "a") (Var "b") ]) (Type "Json.Decode.Decoder" [ Var "b" ]))))
              }
            , { name = "required"
              , comment = """ Decode a required field.

    import Json.Decode as Decode exposing (Decoder, int, string)
    import Json.Decode.Pipeline exposing (required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> required "name" string
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            \"\"\"
          {"id": 123, "email": "sam@example.com", "name": "Sam"}
        \"\"\"


    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }

"""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Lambda (Var "a") (Var "b") ]) (Type "Json.Decode.Decoder" [ Var "b" ])))
              }
            , { name = "requiredAt"
              , comment = """ Decode a required nested field.
"""
              , tipe = Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Lambda (Var "a") (Var "b") ]) (Type "Json.Decode.Decoder" [ Var "b" ])))
              }
            , { name = "resolve"
              , comment = """ Convert a `Decoder (Result x a)` into a `Decoder a`. Useful when you want
to perform some custom processing just before completing the decoding operation.

    import Json.Decode as Decode exposing (Decoder, float, int, string)
    import Json.Decode.Pipeline exposing (required, resolve)

    type alias User =
        { id : Int
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        let
            -- toDecoder gets run *after* all the
            -- (|> required ...) steps are done.
            toDecoder : Int -> String -> Int -> Decoder User
            toDecoder id email version =
                if version > 2 then
                    Decode.succeed (User id email)
                else
                    fail "This JSON is from a deprecated source. Please upgrade!"
        in
        Decode.succeed toDecoder
            |> required "id" int
            |> required "email" string
            |> required "version" int
            -- version is part of toDecoder,
            |> resolve


    -- but it is not a part of User

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            \"\"\"
          {"id": 123, "email": "sam@example.com", "version": 1}
        \"\"\"


    -- Err "This JSON is from a deprecated source. Please upgrade!"

"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Type "Json.Decode.Decoder" [ Var "a" ] ]) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            ]
      }
    ]


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafePackageName packageName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeModuleName : String -> Elm.Module.Name
unsafeModuleName moduleName =
    case Elm.Module.fromString moduleName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeModuleName moduleName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeConstraint : String -> Elm.Constraint.Constraint
unsafeConstraint constraint =
    case Elm.Constraint.fromString constraint of
        Just constr ->
            constr

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeConstraint constraint
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity
