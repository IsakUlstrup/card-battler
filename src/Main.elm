module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, h3, main_, p, text)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )

        Reset ->
            ( 0, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h3 [] [ text "Elm counter" ]
        , p [] [ text <| String.fromInt model ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

