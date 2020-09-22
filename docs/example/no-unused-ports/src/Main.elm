module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Ports exposing (play)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Count Int


type Msg
    = GotAction String
    | Play
    | Stop



--- BASIC METHODS, NO USED PORTS HERE


init : () -> ( Model, Cmd Msg )
init _ =
    ( Count 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view (Count count) =
    Html.a [] [ Html.text ("Count: " ++ String.fromInt count) ]



--- BASIC METHODS THAT USE PORTS


usedInit : () -> ( Model, Cmd Msg )
usedInit _ =
    ( Count 0, Ports.load )


usedUpdate : Msg -> Model -> ( Model, Cmd Msg )
usedUpdate msg model =
    case msg of
        Play ->
            ( model, Ports.play )

        Stop ->
            ( model, Ports.stop )

        _ ->
            ( model, Cmd.none )


usedSubscriptions : Model -> Sub Msg
usedSubscriptions (Count count) =
    if count > 0 then
        Ports.action GotAction

    else
        Sub.none



--- INTENTIONALLY UNUSED CODE


unusedCmd : Cmd msg
unusedCmd =
    play


unusedSub : Model -> Sub Msg
unusedSub _ =
    Ports.action GotAction
