module Main exposing (Model, Msg, main)

import Browser
import Grid exposing (Grid, Point)
import Html exposing (Html, main_)
import Html.Attributes
import Random exposing (Generator, Seed)
import Render exposing (Config)
import Svg exposing (Svg)
import Svg.Attributes


{-| Generate a maybe tiles based on emptyChance percentage
-}
randomTile : Float -> a -> Generator (Maybe a)
randomTile emptyChance tile =
    let
        clampedChance =
            clamp 0 100 emptyChance
    in
    Random.weighted ( 100 - clampedChance, Just tile ) [ ( clampedChance, Nothing ) ]


{-| Generate a circle with some tiles missing
-}
randomCircle : Int -> Point -> a -> Seed -> ( Seed, List ( Point, a ) )
randomCircle radius center tile seed =
    let
        helper position ( s, accum ) =
            let
                ( maybeTile, newSeed ) =
                    Random.step (randomTile 30 tile) s
            in
            case maybeTile of
                Just justTile ->
                    ( newSeed, ( position, justTile ) :: accum )

                Nothing ->
                    ( newSeed, accum )
    in
    List.foldl helper ( seed, [] ) (Grid.circle radius center)



-- TILE


type Tile
    = PlayerTile
    | EnemyTile


tileToString : Tile -> String
tileToString tile =
    case tile of
        PlayerTile ->
            "playerTile"

        EnemyTile ->
            "enemyTile"



-- MODEL


type alias Model =
    { map : Grid Tile
    , animals : Grid Char
    , config : Config
    , seed : Seed
    }


init : Int -> ( Model, Cmd Msg )
init timestamp =
    let
        ( newSeed, playerTiles ) =
            randomCircle 2 ( -1, -1, 2 ) PlayerTile (Random.initialSeed timestamp)

        ( newSeed2, enemyTiles ) =
            randomCircle 1 ( 2, 2, -4 ) EnemyTile newSeed
    in
    ( Model
        (Grid.fromList
            (playerTiles ++ enemyTiles)
        )
        (Grid.fromList
            []
        )
        (Render.initConfig
            |> Render.withPointyTop
            |> Render.withZoom 4
        )
        newSeed2
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


viewHex : Bool -> ( Point, Tile ) -> Svg msg
viewHex flatTop ( position, tile ) =
    Svg.g []
        [ Render.renderHex flatTop
            [ Svg.Attributes.class (tileToString tile)
            , Svg.Attributes.class "tile"
            ]
        , Svg.text_
            [ Svg.Attributes.x "25px"
            , Svg.Attributes.y "25px"
            , Svg.Attributes.textAnchor "middle"
            ]
            [ Svg.text (Grid.pointToString position) ]
        ]


viewAnimal : ( Point, Char ) -> Svg msg
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


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
