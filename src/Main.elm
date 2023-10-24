module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
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


{-| Check if the given position on a tile grid exists and is a player tile
-}
tileIsOpen : Point -> Grid Tile -> Bool
tileIsOpen position tiles =
    case Grid.get position tiles of
        Just PlayerTile ->
            True

        _ ->
            False


{-| Check if the given position on a card grid is empty
-}
cardIsOpen : Point -> Grid Card -> Bool
cardIsOpen position cards =
    case Grid.get position cards of
        Nothing ->
            True

        _ ->
            False


placeCard : Card -> Grid Tile -> Grid Card -> Seed -> ( Seed, Grid Card )
placeCard card tiles cards seed =
    let
        openTiles =
            Grid.keys tiles
                |> List.filter (\t -> tileIsOpen t tiles)
                |> List.filter (\t -> cardIsOpen t cards)

        ( randomInt, newSeed ) =
            Random.step (Random.int 0 (List.length openTiles - 1)) seed
    in
    case openTiles |> List.drop randomInt |> List.head of
        Just tile ->
            ( newSeed, Grid.insert tile card cards )

        Nothing ->
            ( seed, cards )



-- CARD


type alias Card =
    Char



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



-- TURN


type TurnState
    = PlaceCards (List Card) ( Float, Float )
    | DonePlacingCards



-- MODEL


type alias Model =
    { map : Grid Tile
    , animals : Grid Card
    , deck : List Card
    , turnState : TurnState
    , config : Config
    , seed : Seed
    }


init : Int -> ( Model, Cmd Msg )
init timestamp =
    let
        flatTopHexes : Bool
        flatTopHexes =
            False

        ( playerTilesCenter, enemyTilesCenter ) =
            if flatTopHexes then
                ( ( 0, -2, 2 ), ( 0, 3, -3 ) )

            else
                ( ( -1, -1, 2 ), ( 2, 2, -4 ) )

        ( newSeed, playerTiles ) =
            randomCircle 2 playerTilesCenter PlayerTile (Random.initialSeed timestamp)

        ( newSeed2, enemyTiles ) =
            randomCircle 1 enemyTilesCenter EnemyTile newSeed

        grid : Grid Tile
        grid =
            Grid.fromList
                (playerTiles ++ enemyTiles)

        playerDeck : List Card
        playerDeck =
            [ '🐼', '🐻', '🦅', '🦖' ]
    in
    ( Model
        grid
        (Grid.fromList [])
        playerDeck
        (PlaceCards playerDeck ( 200, 200 ))
        (Render.initConfig
            |> Render.withZoom 4
            |> (\config ->
                    if not flatTopHexes then
                        Render.withPointyTop config

                    else
                        config
               )
        )
        newSeed2
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            case model.turnState of
                PlaceCards [] _ ->
                    { model | turnState = DonePlacingCards }

                PlaceCards (c :: cs) ( cd, maxCd ) ->
                    if cd == 0 then
                        let
                            ( newSeed, cards ) =
                                placeCard c model.map model.animals model.seed
                        in
                        { model
                            | turnState = PlaceCards cs ( maxCd, maxCd )
                            , animals = cards
                            , seed = newSeed
                        }

                    else
                        { model | turnState = PlaceCards (c :: cs) ( max 0 (cd - dt), maxCd ) }

                DonePlacingCards ->
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
            , Svg.Attributes.class "hex"
            ]

        -- , Svg.text_
        --     [ Svg.Attributes.x "25px"
        --     , Svg.Attributes.y "25px"
        --     , Svg.Attributes.textAnchor "middle"
        --     ]
        --     [ Svg.text (Grid.pointToString position) ]
        ]


viewAnimal : ( Point, Char ) -> Svg msg
viewAnimal ( _, animal ) =
    Svg.g [ Svg.Attributes.class "animal" ]
        [ Render.renderHex False
            [ Svg.Attributes.class "cooldown-indicator"
            ]
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.fontSize "5rem"
            , Svg.Attributes.x "25px"
            , Svg.Attributes.y "25px"
            ]
            [ Svg.text (String.fromChar animal) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 2000 >> Tick)



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
