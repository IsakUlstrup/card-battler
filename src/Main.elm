module Main exposing (Model, Msg, main)

import Browser
import Grid exposing (Grid)
import Html exposing (Html, main_)
import Html.Attributes
import Render exposing (Config)
import Svg exposing (Svg)



-- MODEL


type alias Model =
    { grid : Grid ()
    , config : Config
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Grid.fromList
            [ ( ( 0, 0, 0 ), () )
            , ( ( 1, 0, -1 ), () )
            , ( ( 0, -1, 1 ), () )
            , ( ( 0, 1, -1 ), () )
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
            [ Render.viewGrid model.config model.grid (viewHex model.config)
            ]
        ]


viewHex : Config -> ( Grid.Point, () ) -> Svg msg
viewHex config _ =
    Render.renderHex config []



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
