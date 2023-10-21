module Main exposing (Model, Msg, main)

import Browser
import Grid exposing (Grid)
import Html exposing (Html, main_)
import Html.Attributes
import Render
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    Grid ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( Grid.fromList
        [ ( ( 0, 0, 0 ), () )
        , ( ( 2, 0, -2 ), () )
        ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Render.customSvg Render.initConfig
            [ Render.viewGrid Render.initConfig model viewHex
            ]
        ]


viewHex : ( Grid.Point, () ) -> Svg msg
viewHex _ =
    Svg.text_ [ Svg.Attributes.fill "white" ] [ Svg.text "o" ]



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
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
