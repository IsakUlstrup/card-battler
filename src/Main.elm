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


{-| Check if the given position on a tile grid exists
-}
tileIsOpen : Grid Tile -> Point -> Bool
tileIsOpen tiles position =
    case Grid.get position tiles of
        Just _ ->
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
                |> List.filter
                    (tileIsOpen tiles)
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


type SkillEffect
    = Hit Int


tickCardCooldown : Float -> Card -> Card
tickCardCooldown dt card =
    if card.health > 0 then
        { card | cooldown = Tuple.mapFirst (\cd -> max 0 (cd - dt)) card.cooldown }

    else
        card


resetCooldown : Card -> Card
resetCooldown card =
    { card | cooldown = Tuple.mapFirst (always (Tuple.second card.cooldown)) card.cooldown }


cooldownIsDone : Card -> Bool
cooldownIsDone card =
    Tuple.first card.cooldown == 0


isAlive : Card -> Bool
isAlive card =
    card.health > 0


hit : Int -> Card -> Card
hit amount target =
    { target | health = max 0 (target.health - amount) }


applySkillEffect : SkillEffect -> Card -> Card
applySkillEffect effect card =
    case effect of
        Hit power ->
            hit power card


hitEffect : Card -> SkillEffect
hitEffect card =
    Hit card.power


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
    | PlayerCardAction ( Point, SkillEffect ) Point Float
    | EnemyCardAction ( Point, SkillEffect ) Point Float
    | Idle



-- MODEL


type alias Model =
    { playerMap : Grid Tile
    , playerCards : Grid Card
    , playerDeck : List Card
    , enemyMap : Grid Tile
    , enemyCards : Grid Card
    , enemyDeck : List Card
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
        (Grid.fromList playerTiles)
        (Grid.fromList [])
        playerDeck
        (Grid.fromList enemyTiles)
        (Grid.fromList [])
        enemyDeck
        (PlaceEnemyCards enemyDeck ( 100, 100 ))
        (Render.initConfig |> Render.withZoom 4)
        newSeed2
    , Cmd.none
    )



-- UPDATE


handleTurnState : Model -> Model
handleTurnState model =
    case model.turnState of
        PlaceEnemyCards [] _ ->
            { model | turnState = PlacePlayerCards model.playerDeck ( 100, 100 ) }

        PlaceEnemyCards (c :: cs) ( cd, maxCd ) ->
            if cd == 0 then
                let
                    ( newSeed, cards ) =
                        placeCard c model.enemyMap model.enemyCards model.seed
                in
                { model
                    | turnState = PlaceEnemyCards cs ( maxCd, maxCd )
                    , enemyCards = cards
                    , seed = newSeed
                }

            else
                model

        PlacePlayerCards [] _ ->
            { model | turnState = Idle }

        PlacePlayerCards (c :: cs) ( cd, maxCd ) ->
            if cd == 0 then
                let
                    ( newSeed, cards ) =
                        placeCard c model.playerMap model.playerCards model.seed
                in
                { model
                    | turnState = PlacePlayerCards cs ( maxCd, maxCd )
                    , playerCards = cards
                    , seed = newSeed
                }

            else
                model

        PlayerCardAction ( pos, effect ) t cd ->
            if cd == 0 then
                { model
                    | playerCards = Grid.update resetCooldown pos model.playerCards
                    , enemyCards = Grid.update (applySkillEffect effect) t model.enemyCards
                }
                    |> setCardActionState

            else
                model

        EnemyCardAction ( pos, effect ) t cd ->
            if cd == 0 then
                { model
                    | enemyCards = Grid.update resetCooldown pos model.enemyCards
                    , playerCards = Grid.update (applySkillEffect effect) t model.playerCards
                }
                    |> setCardActionState

            else
                model

        Idle ->
            setCardActionState model


tickTurnState : Float -> Model -> Model
tickTurnState dt model =
    case model.turnState of
        PlaceEnemyCards cards ( cd, maxCd ) ->
            { model | turnState = PlaceEnemyCards cards ( max 0 (cd - dt), maxCd ) }

        PlacePlayerCards cards ( cd, maxCd ) ->
            { model | turnState = PlacePlayerCards cards ( max 0 (cd - dt), maxCd ) }

        PlayerCardAction action t cd ->
            { model | turnState = PlayerCardAction action t (max 0 (cd - dt)) }

        EnemyCardAction action t cd ->
            { model | turnState = EnemyCardAction action t (max 0 (cd - dt)) }

        _ ->
            model


setCardActionState : Model -> Model
setCardActionState model =
    let
        getDone cards =
            cards |> Grid.toList |> List.filter (\( _, c ) -> cooldownIsDone c) |> List.head

        getTarget cards =
            cards |> Grid.toList |> List.filter (Tuple.second >> isAlive) |> List.head
    in
    case ( getDone model.playerCards, getTarget model.enemyCards ) of
        ( Just c, Just t ) ->
            { model | turnState = PlayerCardAction ( Tuple.first c, hitEffect (Tuple.second c) ) (Tuple.first t) 1000 }

        _ ->
            case ( getDone model.enemyCards, getTarget model.playerCards ) of
                ( Just c, Just t ) ->
                    { model | turnState = EnemyCardAction ( Tuple.first c, hitEffect (Tuple.second c) ) (Tuple.first t) 1000 }

                _ ->
                    { model | turnState = Idle }


tickCardCooldowns : Float -> Model -> Model
tickCardCooldowns dt model =
    case model.turnState of
        Idle ->
            { model
                | playerCards = Grid.map (\_ c -> tickCardCooldown dt c) model.playerCards
                , enemyCards = Grid.map (\_ c -> tickCardCooldown dt c) model.enemyCards
            }

        _ ->
            model


removeDead : Model -> Model
removeDead model =
    { model
        | playerCards = Grid.filter (\_ card -> isAlive card) model.playerCards
        , enemyCards = Grid.filter (\_ card -> isAlive card) model.enemyCards
    }


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickTurnState dt
                |> handleTurnState
                |> tickCardCooldowns dt
                |> removeDead



-- VIEW


activeCard : TurnState -> Maybe ( Point, Point )
activeCard turn =
    case turn of
        PlayerCardAction ( position, _ ) target _ ->
            Just ( position, target )

        EnemyCardAction ( position, _ ) target _ ->
            Just ( position, target )

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Render.customSvg model.config
            [ Svg.Lazy.lazy2 Render.viewGrid viewHex model.playerMap
            , Svg.Lazy.lazy2 Render.viewGrid viewHex model.enemyMap
            , Render.viewGrid (viewCard (activeCard model.turnState)) model.playerCards
            , Render.viewGrid (viewCard (activeCard model.turnState)) model.enemyCards
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


viewCard : Maybe ( Point, Point ) -> ( Point, Card ) -> Svg msg
viewCard selected ( position, card ) =
    let
        isSelected =
            case selected of
                Just ( selectedPosition, _ ) ->
                    selectedPosition == position

                Nothing ->
                    False

        isTargeted =
            case selected of
                Just ( _, target ) ->
                    target == position

                Nothing ->
                    False
    in
    Svg.g
        [ Svg.Attributes.class "card"
        , Render.classList
            [ ( "ready", cooldownIsDone card )
            , ( "selected", isSelected )
            , ( "targeted", isTargeted )
            ]
        ]
        [ Render.renderHex
            [ Svg.Attributes.class "cooldown-indicator"
            , Svg.Attributes.style ("transform: scale(" ++ (1 - (Tuple.first card.cooldown / Tuple.second card.cooldown) |> String.fromFloat) ++ ")")
            ]
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.fontSize "4.5rem"
            , Svg.Attributes.y "10px"
            , Svg.Attributes.class "icon"
            ]
            [ Svg.text (String.fromChar card.icon) ]
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.y "25px"
            , Svg.Attributes.class "stats"
            ]
            [ Svg.text ("❊" ++ String.fromInt card.power ++ " ♥︎" ++ String.fromInt card.health) ]
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
