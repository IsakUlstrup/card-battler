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
import Svg.Lazy


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


{-| Check if the given position on a tile grid exists and is a enemy tile
-}
enemyTileIsOpen : Point -> Grid Tile -> Bool
enemyTileIsOpen position tiles =
    case Grid.get position tiles of
        Just EnemyTile ->
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


placeCard : Bool -> Card -> Grid Tile -> Grid Card -> Seed -> ( Seed, Grid Card )
placeCard player card tiles cards seed =
    let
        openTiles =
            Grid.keys tiles
                |> List.filter
                    (\t ->
                        if player then
                            tileIsOpen t tiles

                        else
                            enemyTileIsOpen t tiles
                    )
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
    { icon : Char
    , cooldown : ( Float, Float )
    }


tickCardCooldown : Float -> Card -> Card
tickCardCooldown dt card =
    if Tuple.first card.cooldown == 0 then
        { card | cooldown = Tuple.mapFirst (always (Tuple.second card.cooldown)) card.cooldown }

    else
        { card | cooldown = Tuple.mapFirst (\cd -> max 0 (cd - dt)) card.cooldown }


playerDeck : List Card
playerDeck =
    [ Card '🐼' ( 2000, 2000 )
    , Card '🐻' ( 3000, 3000 )
    , Card '🦅' ( 1000, 1000 )
    , Card '🦖' ( 1400, 1400 )
    ]


enemyDeck : List Card
enemyDeck =
    [ Card '🦡' ( 1200, 1200 ), Card '🦔' ( 500, 500 ) ]



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
    = PlaceEnemyCards (List Card) ( Float, Float )
    | PlacePlayerCards (List Card) ( Float, Float )
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
    in
    ( Model
        grid
        (Grid.fromList [])
        playerDeck
        (PlaceEnemyCards enemyDeck ( 200, 200 ))
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


tickTurnState : Float -> Model -> Model
tickTurnState dt model =
    case model.turnState of
        PlaceEnemyCards [] _ ->
            { model | turnState = PlacePlayerCards model.deck ( 200, 200 ) }

        PlaceEnemyCards (c :: cs) ( cd, maxCd ) ->
            if cd == 0 then
                let
                    ( newSeed, cards ) =
                        placeCard False c model.map model.animals model.seed
                in
                { model
                    | turnState = PlaceEnemyCards cs ( maxCd, maxCd )
                    , animals = cards
                    , seed = newSeed
                }

            else
                { model | turnState = PlaceEnemyCards (c :: cs) ( max 0 (cd - dt), maxCd ) }

        PlacePlayerCards [] _ ->
            { model | turnState = DonePlacingCards }

        PlacePlayerCards (c :: cs) ( cd, maxCd ) ->
            if cd == 0 then
                let
                    ( newSeed, cards ) =
                        placeCard True c model.map model.animals model.seed
                in
                { model
                    | turnState = PlacePlayerCards cs ( maxCd, maxCd )
                    , animals = cards
                    , seed = newSeed
                }

            else
                { model | turnState = PlacePlayerCards (c :: cs) ( max 0 (cd - dt), maxCd ) }

        DonePlacingCards ->
            model


tickCardCooldowns : Float -> Model -> Model
tickCardCooldowns dt model =
    case model.turnState of
        DonePlacingCards ->
            { model | animals = Grid.map (\_ c -> tickCardCooldown dt c) model.animals }

        _ ->
            model


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickTurnState dt
                |> tickCardCooldowns dt



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Render.customSvg model.config
            [ Svg.Lazy.lazy3 Render.viewGrid viewHex model.config model.map
            , Render.viewGrid viewAnimal model.config model.animals
            ]
        ]


viewHex : ( Point, Tile ) -> Svg msg
viewHex ( position, tile ) =
    Svg.g []
        [ Render.renderHex False
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


viewAnimal : ( Point, Card ) -> Svg msg
viewAnimal ( _, animal ) =
    Svg.g [ Svg.Attributes.class "animal" ]
        [ Render.renderHex False
            [ Svg.Attributes.class "cooldown-indicator"
            , Svg.Attributes.style ("transform: scale(" ++ (1 - (Tuple.first animal.cooldown / Tuple.second animal.cooldown) |> String.fromFloat) ++ ")")
            ]
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.fontSize "5rem"
            ]
            [ Svg.text (String.fromChar animal.icon) ]
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
