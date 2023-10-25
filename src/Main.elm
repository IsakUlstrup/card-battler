module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Grid exposing (Grid, Point)
import Html exposing (Html, main_)
import Html.Attributes
import Random exposing (Seed)
import Render exposing (Config)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy


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
    , power : Int
    , health : Int
    }


tickCardCooldown : Float -> Card -> Card
tickCardCooldown dt card =
    if Tuple.first card.cooldown == 0 then
        { card | cooldown = Tuple.mapFirst (always (Tuple.second card.cooldown)) card.cooldown }

    else
        { card | cooldown = Tuple.mapFirst (\cd -> max 0 (cd - dt)) card.cooldown }


cooldownIsDone : Card -> Bool
cooldownIsDone card =
    Tuple.first card.cooldown == 0


playerDeck : List Card
playerDeck =
    [ Card '🐼' ( 4000, 4000 ) 7 5
    , Card '🐻' ( 6000, 6000 ) 8 7
    , Card '🦅' ( 2000, 2000 ) 4 3
    , Card '🦖' ( 2800, 2800 ) 9 8
    ]


enemyDeck : List Card
enemyDeck =
    [ Card '🦡' ( 2400, 2400 ) 7 4
    , Card '🦔' ( 2000, 2000 ) 2 5
    ]



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
    | CardAction Point Float
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
        ( playerTilesCenter, enemyTilesCenter ) =
            ( ( -1, -1, 2 ), ( 2, 2, -4 ) )

        ( newSeed, playerTiles ) =
            Grid.randomCircle 2 playerTilesCenter PlayerTile (Random.initialSeed timestamp)

        ( newSeed2, enemyTiles ) =
            Grid.randomCircle 1 enemyTilesCenter EnemyTile newSeed
    in
    ( Model
        (Grid.fromList (playerTiles ++ enemyTiles))
        (Grid.fromList [])
        playerDeck
        (PlaceEnemyCards enemyDeck ( 100, 100 ))
        (Render.initConfig |> Render.withZoom 4)
        newSeed2
    , Cmd.none
    )



-- UPDATE


tickTurnState : Float -> Model -> Model
tickTurnState dt model =
    case model.turnState of
        PlaceEnemyCards [] _ ->
            { model | turnState = PlacePlayerCards model.deck ( 100, 100 ) }

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

        CardAction cardPosition cd ->
            if cd > 0 then
                { model | turnState = CardAction cardPosition (max 0 (cd - dt)) }

            else
                { model | turnState = DonePlacingCards }

        DonePlacingCards ->
            case model.animals |> Grid.toList |> List.filter (\( _, c ) -> cooldownIsDone c) of
                c :: _ ->
                    { model | turnState = CardAction (Tuple.first c) 1000 }

                _ ->
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


activeCard : TurnState -> Maybe Point
activeCard turn =
    case turn of
        CardAction position _ ->
            Just position

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Render.customSvg model.config
            [ Svg.Lazy.lazy2 Render.viewGrid viewHex model.map
            , Render.viewGrid (viewAnimal (activeCard model.turnState)) model.animals
            ]
        ]


viewHex : ( Point, Tile ) -> Svg msg
viewHex ( position, tile ) =
    Svg.g []
        [ Render.renderHex
            [ Svg.Attributes.class (tileToString tile)
            , Svg.Attributes.class "hex"
            ]
        ]


viewAnimal : Maybe Point -> ( Point, Card ) -> Svg msg
viewAnimal selected ( position, animal ) =
    let
        isSelected =
            case selected of
                Just selectedPosition ->
                    selectedPosition == position

                Nothing ->
                    False
    in
    Svg.g
        [ Svg.Attributes.class "animal"
        , Render.classList
            [ ( "ready", cooldownIsDone animal )
            , ( "selected", isSelected )
            ]
        ]
        [ Render.renderHex
            [ Svg.Attributes.class "cooldown-indicator"
            , Svg.Attributes.style ("transform: scale(" ++ (1 - (Tuple.first animal.cooldown / Tuple.second animal.cooldown) |> String.fromFloat) ++ ")")
            ]
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.fontSize "4.5rem"
            , Svg.Attributes.y "10px"
            , Svg.Attributes.class "icon"
            ]
            [ Svg.text (String.fromChar animal.icon) ]
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.y "25px"
            , Svg.Attributes.class "stats"
            ]
            [ Svg.text ("❊" ++ String.fromInt animal.power ++ " ♥︎" ++ String.fromInt animal.health) ]
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
