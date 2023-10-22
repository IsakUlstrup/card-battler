module Main exposing (Model, Msg, main)

import Browser
import Grid exposing (Grid)
import Html exposing (Html, main_)
import Html.Attributes
import Render exposing (Config)
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { map : Grid ()
    , animals : Grid Char
    , config : Config
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Grid.fromList
            (Grid.circle 2 ( 0, 0, 0 ) |> List.map (\p -> ( p, () )))
        )
        (Grid.fromList
            [ ( ( 0, 0, 0 ), '🐼' )
            , ( ( 1, 0, -1 ), '🦝' )
            , ( ( 0, 1, -1 ), '🌳' )
            ]
        )
        (Render.initConfig
            |> Render.withPointyTop
            |> Render.withZoom 4
        )
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
        [ Render.customSvg model.config
            [ Render.viewGrid model.config model.map (viewHex model.config.hexFlatTop)
            , Render.viewGrid model.config model.animals viewAnimal
            ]
        ]


viewHex : Bool -> ( Grid.Point, () ) -> Svg msg
viewHex flatTop _ =
    Render.renderHex flatTop []


viewAnimal : ( Grid.Point, Char ) -> Svg msg
viewAnimal ( _, animal ) =
    Svg.text_
        [ Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.fontSize "5rem"
        , Svg.Attributes.x "25px"
        , Svg.Attributes.y "25px"
        , Svg.Attributes.class "animal"
        ]
        [ Svg.text (String.fromChar animal) ]



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
