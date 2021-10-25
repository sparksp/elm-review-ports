module Review.Test.Dependencies.ElmJson exposing (dependency)

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
    Dependency.create "elm/json"
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint "0.19.0 <= v < 0.20.0"
        , exposed = Elm.Project.ExposedList [ unsafeModuleName "Json.Decode", unsafeModuleName "Json.Encode" ]
        , license = Elm.License.fromString "BSD-3-Clause" |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName "elm/json"
        , summary = "Encode and decode JSON values"
        , deps = [ ( unsafePackageName "elm/core", unsafeConstraint "1.0.0 <= v < 2.0.0" ) ]
        , testDeps = []
        , version = Elm.Version.fromString "1.1.3" |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ { name = "Json.Decode"
      , comment = """ Turn JSON values into Elm values. Definitely check out this [intro to
JSON decoders][guide] to get a feel for how this library works!

[guide]: https://guide.elm-lang.org/effects/json.html

# Primitives
@docs Decoder, string, bool, int, float

# Data Structures
@docs nullable, list, array, dict, keyValuePairs, oneOrMore

# Object Primitives
@docs field, at, index

# Inconsistent Structure
@docs maybe, oneOf

# Run Decoders
@docs decodeString, decodeValue, Value, Error, errorToString

# Mapping

**Note:** If you run out of map functions, take a look at [elm-json-decode-pipeline][pipe]
which makes it easier to handle large objects, but produces lower quality type
errors.

[pipe]: /packages/NoRedInk/elm-json-decode-pipeline/latest

@docs map, map2, map3, map4, map5, map6, map7, map8

# Fancy Decoding
@docs lazy, value, null, succeed, fail, andThen
"""
      , aliases =
            [ { name = "Value"
              , args = []
              , comment = """ Represents a JavaScript value.
"""
              , tipe = Type "Json.Encode.Value" []
              }
            ]
      , unions =
            [ { name = "Decoder"
              , args = [ "a" ]
              , comment = """ A value that knows how to decode JSON values.

There is a whole section in `guide.elm-lang.org` about decoders, so [check it
out](https://guide.elm-lang.org/interop/json.html) for a more comprehensive
introduction!
"""
              , tags = []
              }
            , { name = "Error"
              , args = []
              , comment = """ A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire JSON object and show the part causing the failure in
red.
"""
              , tags =
                    [ ( "Field", [ Type "String.String" [], Type "Json.Decode.Error" [] ] )
                    , ( "Index", [ Type "Basics.Int" [], Type "Json.Decode.Error" [] ] )
                    , ( "OneOf", [ Type "List.List" [ Type "Json.Decode.Error" [] ] ] )
                    , ( "Failure", [ Type "String.String" [], Type "Json.Decode.Value" [] ] )
                    ]
              }
            ]
      , binops = []
      , values =
            [ { name = "andThen"
              , comment = """ Create decoders that depend on previous results. If you are creating
versioned data, you might do something like this:

    info : Decoder Info
    info =
      field "version" int
        |> andThen infoHelp

    infoHelp : Int -> Decoder Info
    infoHelp version =
      case version of
        4 ->
          infoDecoder4

        3 ->
          infoDecoder3

        _ ->
          fail <|
            "Trying to decode info, but version "
            ++ toString version ++ " is not supported."

    -- infoDecoder4 : Decoder Info
    -- infoDecoder3 : Decoder Info
"""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Decode.Decoder" [ Var "b" ])) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "b" ]))
              }
            , { name = "array"
              , comment = """ Decode a JSON array into an Elm `Array`.

    decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
    decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Array.Array" [ Var "a" ] ])
              }
            , { name = "at"
              , comment = """ Decode a nested JSON object, requiring certain fields.

    json = \"\"\"{ "person": { "name": "tom", "age": 42 } }\"\"\"

    decodeString (at ["person", "name"] string) json  == Ok "tom"
    decodeString (at ["person", "age" ] int   ) json  == Ok "42

This is really just a shorthand for saying things like:

    field "person" (field "name" string) == at ["person","name"] string
"""
              , tipe = Lambda (Type "List.List" [ Type "String.String" [] ]) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "a" ]))
              }
            , { name = "bool"
              , comment = """ Decode a JSON boolean into an Elm `Bool`.

    decodeString bool "true"              == Ok True
    decodeString bool "42"                == Err ...
    decodeString bool "3.14"              == Err ...
    decodeString bool "\\"hello\\""         == Err ...
    decodeString bool "{ \\"hello\\": 42 }" == Err ...
"""
              , tipe = Type "Json.Decode.Decoder" [ Type "Basics.Bool" [] ]
              }
            , { name = "decodeString"
              , comment = """ Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder`
fails for some reason.

    decodeString int "4"     == Ok 4
    decodeString int "1 + 2" == Err ...
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "String.String" []) (Type "Result.Result" [ Type "Json.Decode.Error" [], Var "a" ]))
              }
            , { name = "decodeValue"
              , comment = """ Run a `Decoder` on some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Value" []) (Type "Result.Result" [ Type "Json.Decode.Error" [], Var "a" ]))
              }
            , { name = "dict"
              , comment = """ Decode a JSON object into an Elm `Dict`.

    decodeString (dict int) "{ \\"alice\\": 42, \\"bob\\": 99 }"
      == Ok (Dict.fromList [("alice", 42), ("bob", 99)])

If you need the keys (like `"alice"` and `"bob"`) available in the `Dict`
values as well, I recommend using a (private) intermediate data structure like
`Info` in this example:

    module User exposing (User, decoder)

    import Dict
    import Json.Decode exposing (..)

    type alias User =
      { name : String
      , height : Float
      , age : Int
      }

    decoder : Decoder (Dict.Dict String User)
    decoder =
      map (Dict.map infoToUser) (dict infoDecoder)

    type alias Info =
      { height : Float
      , age : Int
      }

    infoDecoder : Decoder Info
    infoDecoder =
      map2 Info
        (field "height" float)
        (field "age" int)

    infoToUser : String -> Info -> User
    infoToUser name { height, age } =
      User name height age

So now JSON like `{ "alice": { height: 1.6, age: 33 }}` are turned into
dictionary values like `Dict.singleton "alice" (User "alice" 1.6 33)` if
you need that.
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Dict.Dict" [ Type "String.String" [], Var "a" ] ])
              }
            , { name = "errorToString"
              , comment = """ Convert a decoding error into a `String` that is nice for debugging.

It produces multiple lines of output, so you may want to peek at it with
something like this:

    import Html
    import Json.Decode as Decode

    errorToHtml : Decode.Error -> Html.Html msg
    errorToHtml error =
      Html.pre [] [ Html.text (Decode.errorToString error) ]

**Note:** It would be cool to do nicer coloring and fancier HTML, but I wanted
to avoid having an `elm/html` dependency for now. It is totally possible to
crawl the `Error` structure and create this separately though!
"""
              , tipe = Lambda (Type "Json.Decode.Error" []) (Type "String.String" [])
              }
            , { name = "fail"
              , comment = """ Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.
"""
              , tipe = Lambda (Type "String.String" []) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "field"
              , comment = """ Decode a JSON object, requiring a particular field.

    decodeString (field "x" int) "{ \\"x\\": 3 }"            == Ok 3
    decodeString (field "x" int) "{ \\"x\\": 3, \\"y\\": 4 }"  == Ok 3
    decodeString (field "x" int) "{ \\"x\\": true }"         == Err ...
    decodeString (field "x" int) "{ \\"y\\": 4 }"            == Err ...

    decodeString (field "name" string) "{ \\"name\\": \\"tom\\" }" == Ok "tom"

The object *can* have other fields. Lots of them! The only thing this decoder
cares about is if `x` is present and that the value there is an `Int`.

Check out [`map2`](#map2) to see how to decode multiple fields!
"""
              , tipe = Lambda (Type "String.String" []) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "a" ]))
              }
            , { name = "float"
              , comment = """ Decode a JSON number into an Elm `Float`.

    decodeString float "true"              == Err ..
    decodeString float "42"                == Ok 42
    decodeString float "3.14"              == Ok 3.14
    decodeString float "\\"hello\\""         == Err ...
    decodeString float "{ \\"hello\\": 42 }" == Err ...
"""
              , tipe = Type "Json.Decode.Decoder" [ Type "Basics.Float" [] ]
              }
            , { name = "index"
              , comment = """ Decode a JSON array, requiring a particular index.

    json = \"\"\"[ "alice", "bob", "chuck" ]\"\"\"

    decodeString (index 0 string) json  == Ok "alice"
    decodeString (index 1 string) json  == Ok "bob"
    decodeString (index 2 string) json  == Ok "chuck"
    decodeString (index 3 string) json  == Err ...
"""
              , tipe = Lambda (Type "Basics.Int" []) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "a" ]))
              }
            , { name = "int"
              , comment = """ Decode a JSON number into an Elm `Int`.

    decodeString int "true"              == Err ...
    decodeString int "42"                == Ok 42
    decodeString int "3.14"              == Err ...
    decodeString int "\\"hello\\""         == Err ...
    decodeString int "{ \\"hello\\": 42 }" == Err ...
"""
              , tipe = Type "Json.Decode.Decoder" [ Type "Basics.Int" [] ]
              }
            , { name = "keyValuePairs"
              , comment = """ Decode a JSON object into an Elm `List` of pairs.

    decodeString (keyValuePairs int) "{ \\"alice\\": 42, \\"bob\\": 99 }"
      == Ok [("alice", 42), ("bob", 99)]
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "List.List" [ Tuple [ Type "String.String" [], Var "a" ] ] ])
              }
            , { name = "lazy"
              , comment = """ Sometimes you have JSON with recursive structure, like nested comments.
You can use `lazy` to make sure your decoder unrolls lazily.

    type alias Comment =
      { message : String
      , responses : Responses
      }

    type Responses = Responses (List Comment)

    comment : Decoder Comment
    comment =
      map2 Comment
        (field "message" string)
        (field "responses" (map Responses (list (lazy (\\_ -> comment)))))

If we had said `list comment` instead, we would start expanding the value
infinitely. What is a `comment`? It is a decoder for objects where the
`responses` field contains comments. What is a `comment` though? Etc.

By using `list (lazy (\\_ -> comment))` we make sure the decoder only expands
to be as deep as the JSON we are given. You can read more about recursive data
structures [here][].

[here]: https://github.com/elm/compiler/blob/master/hints/recursive-alias.md
"""
              , tipe = Lambda (Lambda (Tuple []) (Type "Json.Decode.Decoder" [ Var "a" ])) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "list"
              , comment = """ Decode a JSON array into an Elm `List`.

    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
    decodeString (list bool) "[true,false]" == Ok [True,False]
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "List.List" [ Var "a" ] ])
              }
            , { name = "map"
              , comment = """ Transform a decoder. Maybe you just want to know the length of a string:

    import String

    stringLength : Decoder Int
    stringLength =
      map String.length string

It is often helpful to use `map` with `oneOf`, like when defining `nullable`:

    nullable : Decoder a -> Decoder (Maybe a)
    nullable decoder =
      oneOf
        [ null Nothing
        , map Just decoder
        ]
"""
              , tipe = Lambda (Lambda (Var "a") (Var "value")) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))
              }
            , { name = "map2"
              , comment = """ Try two decoders and then combine the result. We can use this to decode
objects with many fields:

    type alias Point = { x : Float, y : Float }

    point : Decoder Point
    point =
      map2 Point
        (field "x" float)
        (field "y" float)

    -- decodeString point \"\"\"{ "x": 3, "y": 4 }\"\"\" == Ok { x = 3, y = 4 }

It tries each individual decoder and puts the result together with the `Point`
constructor.
"""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Var "value"))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))
              }
            , { name = "map3"
              , comment = """ Try three decoders and then combine the result. We can use this to decode
objects with many fields:

    type alias Person = { name : String, age : Int, height : Float }

    person : Decoder Person
    person =
      map3 Person
        (at ["name"] string)
        (at ["info","age"] int)
        (at ["info","height"] float)

    -- json = \"\"\"{ "name": "tom", "info": { "age": 42, "height": 1.8 } }\"\"\"
    -- decodeString person json == Ok { name = "tom", age = 42, height = 1.8 }

Like `map2` it tries each decoder in order and then give the results to the
`Person` constructor. That can be any function though!
"""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Var "value")))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))))
              }
            , { name = "map4"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Var "value"))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))))
              }
            , { name = "map5"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Var "value")))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))))))
              }
            , { name = "map6"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Var "value"))))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "f" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))))))
              }
            , { name = "map7"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Lambda (Var "g") (Var "value")))))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "f" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "g" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))))))))
              }
            , { name = "map8"
              , comment = ""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Var "b") (Lambda (Var "c") (Lambda (Var "d") (Lambda (Var "e") (Lambda (Var "f") (Lambda (Var "g") (Lambda (Var "h") (Var "value"))))))))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "b" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "c" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "d" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "e" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "f" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "g" ]) (Lambda (Type "Json.Decode.Decoder" [ Var "h" ]) (Type "Json.Decode.Decoder" [ Var "value" ])))))))))
              }
            , { name = "maybe"
              , comment = """ Helpful for dealing with optional fields. Here are a few slightly different
examples:

    json = \"\"\"{ "name": "tom", "age": 42 }\"\"\"

    decodeString (maybe (field "age"    int  )) json == Ok (Just 42)
    decodeString (maybe (field "name"   int  )) json == Ok Nothing
    decodeString (maybe (field "height" float)) json == Ok Nothing

    decodeString (field "age"    (maybe int  )) json == Ok (Just 42)
    decodeString (field "name"   (maybe int  )) json == Ok Nothing
    decodeString (field "height" (maybe float)) json == Err ...

Notice the last example! It is saying we *must* have a field named `height` and
the content *may* be a float. There is no `height` field, so the decoder fails.

Point is, `maybe` will make exactly what it contains conditional. For optional
fields, this means you probably want it *outside* a use of `field` or `at`.
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Maybe.Maybe" [ Var "a" ] ])
              }
            , { name = "null"
              , comment = """ Decode a `null` value into some Elm value.

    decodeString (null False) "null" == Ok False
    decodeString (null 42) "null"    == Ok 42
    decodeString (null 42) "42"      == Err ..
    decodeString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.
"""
              , tipe = Lambda (Var "a") (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "nullable"
              , comment = """ Decode a nullable JSON value into an Elm value.

    decodeString (nullable int) "13"    == Ok (Just 13)
    decodeString (nullable int) "42"    == Ok (Just 42)
    decodeString (nullable int) "null"  == Ok Nothing
    decodeString (nullable int) "true"  == Err ..
"""
              , tipe = Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Type "Maybe.Maybe" [ Var "a" ] ])
              }
            , { name = "oneOf"
              , comment = """ Try a bunch of different decoders. This can be useful if the JSON may come
in a couple different formats. For example, say you want to read an array of
numbers, but some of them are `null`.

    import String

    badInt : Decoder Int
    badInt =
      oneOf [ int, null 0 ]

    -- decodeString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]

Why would someone generate JSON like this? Questions like this are not good
for your health. The point is that you can use `oneOf` to handle situations
like this!

You could also use `oneOf` to help version your data. Try the latest format,
then a few older ones that you still support. You could use `andThen` to be
even more particular if you wanted.
"""
              , tipe = Lambda (Type "List.List" [ Type "Json.Decode.Decoder" [ Var "a" ] ]) (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "oneOrMore"
              , comment = """ Decode a JSON array that has one or more elements. This comes up if you
want to enable drag-and-drop of files into your application. You would pair
this function with [`elm/file`]() to write a `dropDecoder` like this:

    import File exposing (File)
    import Json.Decoder as D

    type Msg
      = GotFiles File (List Files)

    inputDecoder : D.Decoder Msg
    inputDecoder =
      D.at ["dataTransfer","files"] (D.oneOrMore GotFiles File.decoder)

This captures the fact that you can never drag-and-drop zero files.
"""
              , tipe = Lambda (Lambda (Var "a") (Lambda (Type "List.List" [ Var "a" ]) (Var "value"))) (Lambda (Type "Json.Decode.Decoder" [ Var "a" ]) (Type "Json.Decode.Decoder" [ Var "value" ]))
              }
            , { name = "string"
              , comment = """ Decode a JSON string into an Elm `String`.

    decodeString string "true"              == Err ...
    decodeString string "42"                == Err ...
    decodeString string "3.14"              == Err ...
    decodeString string "\\"hello\\""         == Ok "hello"
    decodeString string "{ \\"hello\\": 42 }" == Err ...
"""
              , tipe = Type "Json.Decode.Decoder" [ Type "String.String" [] ]
              }
            , { name = "succeed"
              , comment = """ Ignore the JSON and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

This is handy when used with `oneOf` or `andThen`.
"""
              , tipe = Lambda (Var "a") (Type "Json.Decode.Decoder" [ Var "a" ])
              }
            , { name = "value"
              , comment = """ Do not do anything with a JSON value, just bring it into Elm as a `Value`.
This can be useful if you have particularly complex data that you would like to
deal with later. Or if you are going to send it out a port and do not care
about its structure.
"""
              , tipe = Type "Json.Decode.Decoder" [ Type "Json.Decode.Value" [] ]
              }
            ]
      }
    , { name = "Json.Encode"
      , comment = """ Library for turning Elm values into Json values.

# Encoding
@docs encode, Value

# Primitives
@docs string, int, float, bool, null

# Arrays
@docs list, array, set

# Objects
@docs object, dict
"""
      , aliases = []
      , unions =
            [ { name = "Value"
              , args = []
              , comment = """ Represents a JavaScript value.
"""
              , tags = []
              }
            ]
      , binops = []
      , values =
            [ { name = "array"
              , comment = """ Turn an `Array` into a JSON array.
"""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Encode.Value" [])) (Lambda (Type "Array.Array" [ Var "a" ]) (Type "Json.Encode.Value" []))
              }
            , { name = "bool"
              , comment = """ Turn a `Bool` into a JSON boolean.

    import Json.Encode exposing (encode, bool)

    -- encode 0 (bool True)  == "true"
    -- encode 0 (bool False) == "false"
"""
              , tipe = Lambda (Type "Basics.Bool" []) (Type "Json.Encode.Value" [])
              }
            , { name = "dict"
              , comment = """ Turn a `Dict` into a JSON object.

    import Dict exposing (Dict)
    import Json.Encode as Encode

    people : Dict String Int
    people =
      Dict.fromList [ ("Tom",42), ("Sue", 38) ]

    -- Encode.encode 0 (Encode.dict identity Encode.int people)
    --   == \"\"\"{"Tom":42,"Sue":38}\"\"\"
"""
              , tipe = Lambda (Lambda (Var "k") (Type "String.String" [])) (Lambda (Lambda (Var "v") (Type "Json.Encode.Value" [])) (Lambda (Type "Dict.Dict" [ Var "k", Var "v" ]) (Type "Json.Encode.Value" [])))
              }
            , { name = "encode"
              , comment = """ Convert a `Value` into a prettified string. The first argument specifies
the amount of indentation in the resulting string.

    import Json.Encode as Encode

    tom : Encode.Value
    tom =
        Encode.object
            [ ( "name", Encode.string "Tom" )
            , ( "age", Encode.int 42 )
            ]

    compact = Encode.encode 0 tom
    -- {"name":"Tom","age":42}

    readable = Encode.encode 4 tom
    -- {
    --     "name": "Tom",
    --     "age": 42
    -- }
"""
              , tipe = Lambda (Type "Basics.Int" []) (Lambda (Type "Json.Encode.Value" []) (Type "String.String" []))
              }
            , { name = "float"
              , comment = """ Turn a `Float` into a JSON number.

    import Json.Encode exposing (encode, float)

    -- encode 0 (float 3.14)     == "3.14"
    -- encode 0 (float 1.618)    == "1.618"
    -- encode 0 (float -42)      == "-42"
    -- encode 0 (float NaN)      == "null"
    -- encode 0 (float Infinity) == "null"

**Note:** Floating point numbers are defined in the [IEEE 754 standard][ieee]
which is hardcoded into almost all CPUs. This standard allows `Infinity` and
`NaN`. [The JSON spec][json] does not include these values, so we encode them
both as `null`.

[ieee]: https://en.wikipedia.org/wiki/IEEE_754
[json]: https://www.json.org/
"""
              , tipe = Lambda (Type "Basics.Float" []) (Type "Json.Encode.Value" [])
              }
            , { name = "int"
              , comment = """ Turn an `Int` into a JSON number.

    import Json.Encode exposing (encode, int)

    -- encode 0 (int 42) == "42"
    -- encode 0 (int -7) == "-7"
    -- encode 0 (int 0)  == "0"
"""
              , tipe = Lambda (Type "Basics.Int" []) (Type "Json.Encode.Value" [])
              }
            , { name = "list"
              , comment = """ Turn a `List` into a JSON array.

    import Json.Encode as Encode exposing (bool, encode, int, list, string)

    -- encode 0 (list int [1,3,4])       == "[1,3,4]"
    -- encode 0 (list bool [True,False]) == "[true,false]"
    -- encode 0 (list string ["a","b"])  == \"\"\"["a","b"]\"\"\"

"""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Encode.Value" [])) (Lambda (Type "List.List" [ Var "a" ]) (Type "Json.Encode.Value" []))
              }
            , { name = "null"
              , comment = """ Create a JSON `null` value.

    import Json.Encode exposing (encode, null)

    -- encode 0 null == "null"
"""
              , tipe = Type "Json.Encode.Value" []
              }
            , { name = "object"
              , comment = """ Create a JSON object.

    import Json.Encode as Encode

    tom : Encode.Value
    tom =
        Encode.object
            [ ( "name", Encode.string "Tom" )
            , ( "age", Encode.int 42 )
            ]

    -- Encode.encode 0 tom == \"\"\"{"name":"Tom","age":42}\"\"\"
"""
              , tipe = Lambda (Type "List.List" [ Tuple [ Type "String.String" [], Type "Json.Encode.Value" [] ] ]) (Type "Json.Encode.Value" [])
              }
            , { name = "set"
              , comment = """ Turn an `Set` into a JSON array.
"""
              , tipe = Lambda (Lambda (Var "a") (Type "Json.Encode.Value" [])) (Lambda (Type "Set.Set" [ Var "a" ]) (Type "Json.Encode.Value" []))
              }
            , { name = "string"
              , comment = """ Turn a `String` into a JSON string.

    import Json.Encode exposing (encode, string)

    -- encode 0 (string "")      == "\\"\\""
    -- encode 0 (string "abc")   == "\\"abc\\""
    -- encode 0 (string "hello") == "\\"hello\\""
"""
              , tipe = Lambda (Type "String.String" []) (Type "Json.Encode.Value" [])
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
