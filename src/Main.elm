module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Buff exposing (Buff)
import Card exposing (Action(..), Card)
import Character exposing (Character)
import Codec
import Content.Cards as Cards
import Content.Characters as Characters
import Cooldown exposing (Cooldown)
import CustomDict as Dict
import Energy exposing (Energy)
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events
import Html.Keyed
import Stat exposing (Stat)



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    150



-- TURN STATE


characterTypeString : Bool -> String
characterTypeString isPlayer =
    if isPlayer then
        "player"

    else
        "enemy"


type TurnState
    = Recovering
    | Attacking Bool Action Cooldown
    | Defeat
    | Victory (List Card)



-- MODEL


type alias RunState =
    { characters : ( Character, Character )
    , turnState : TurnState
    , encounters : List Character
    }


type alias HomeState =
    { character : Maybe Character
    , deck : List Card
    }


type GameState
    = Run RunState
    | Home HomeState


type alias Model =
    { gameState : GameState
    , cards : List Card
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Home (HomeState Nothing []))
        [ Cards.basicCard, Cards.basicCard, Cards.buffCard ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedNextEnemy
    | ClickedReturnHome
    | ClickedResetGame
    | ClickedPlayerCard Int
    | ClickedReward Card
    | ClickedStartRun Character (List Card)
    | ClickedCardInCollection Int Card
    | ClickedCardInSelectedCards Int Card
    | ClickedCharacterPreset Character


update : Msg -> Model -> Model
update msg model =
    case ( msg, model.gameState ) of
        ( Tick dt, Run runState ) ->
            { model
                | gameState =
                    Run
                        (runState
                            |> tickCharacters dt
                            |> tickTurnState dt
                            |> advanceTurnState
                        )
            }

        ( ClickedNextEnemy, Run runState ) ->
            case List.head runState.encounters of
                Just character ->
                    { model
                        | gameState =
                            Run
                                { runState
                                    | characters =
                                        runState.characters
                                            |> Tuple.mapSecond (always (character |> Character.drawHand 3))
                                            |> Tuple.mapFirst Character.resetCards
                                            |> Tuple.mapFirst (Character.drawHand 5)
                                    , encounters = List.drop 1 runState.encounters
                                    , turnState = Recovering
                                }
                    }

                Nothing ->
                    model

        ( ClickedReturnHome, Run runState ) ->
            { model
                | gameState = Home (HomeState Nothing [])
                , cards = (runState.characters |> Tuple.first |> Character.resetCards |> .deck) ++ model.cards
            }

        ( ClickedResetGame, _ ) ->
            init () |> Tuple.first

        ( ClickedPlayerCard index, Run runState ) ->
            case runState.turnState of
                Recovering ->
                    { model | gameState = Run (playCard True index runState) }

                _ ->
                    model

        ( ClickedReward card, Run runState ) ->
            case runState.turnState of
                Victory _ ->
                    { model
                        | gameState =
                            Run
                                { runState
                                    | turnState = Victory []
                                    , characters = Tuple.mapFirst (Character.addCard card) runState.characters
                                }
                    }

                _ ->
                    model

        ( ClickedStartRun player deck, _ ) ->
            { model
                | gameState =
                    Run
                        (RunState
                            ( player |> Character.setDeck deck |> Character.drawHand 5
                            , Characters.badger |> Character.drawHand 1
                            )
                            Recovering
                            [ Characters.rabbit, Characters.chick ]
                        )
            }

        ( ClickedCardInCollection index card, Home homeState ) ->
            { model
                | gameState = Home { homeState | deck = card :: homeState.deck }
                , cards = List.take index model.cards ++ List.drop (index + 1) model.cards
            }

        ( ClickedCardInSelectedCards index card, Home homeState ) ->
            { model
                | gameState = Home { homeState | deck = List.take index homeState.deck ++ List.drop (index + 1) homeState.deck }
                , cards = card :: model.cards
            }

        ( ClickedCharacterPreset character, Home homeState ) ->
            { model
                | gameState = Home { homeState | character = Just character }
            }

        _ ->
            model


updateFlag : (Character -> Character) -> Bool -> RunState -> RunState
updateFlag f isPlayer model =
    if isPlayer then
        { model | characters = model.characters |> Tuple.mapFirst f }

    else
        { model | characters = model.characters |> Tuple.mapSecond f }


playCard : Bool -> Int -> RunState -> RunState
playCard isPlayer index model =
    case Character.playCardAtIndex index (Tuple.first model.characters) of
        ( newCharacter, Just action ) ->
            { model | turnState = Attacking isPlayer action (Cooldown.new characterAnimationDuration) }
                |> updateFlag (always newCharacter) isPlayer

        ( _, Nothing ) ->
            model


tickCharacters : Float -> RunState -> RunState
tickCharacters dt model =
    case ( model.turnState, [ Tuple.first model.characters, Tuple.second model.characters ] |> List.all Character.isAlive ) of
        ( Recovering, True ) ->
            { model
                | characters =
                    model.characters
                        |> Tuple.mapBoth (Character.tick dt) (Character.tick dt)
            }

        _ ->
            model


tickTurnState : Float -> RunState -> RunState
tickTurnState dt model =
    case model.turnState of
        Recovering ->
            model

        Attacking isPlayer power cooldown ->
            { model | turnState = Attacking isPlayer power (Cooldown.tick dt cooldown) }

        Defeat ->
            model

        Victory _ ->
            model



-- setHitState : Bool -> Action -> Model -> Model
-- setHitState isPlayer action model =
--     { model
--         | turnState = Hit (not isPlayer) (Cooldown.new characterAnimationDuration)
--     }
--         |> updateFlag (Character.applyAction action) (not isPlayer)


setRecoveringState : RunState -> RunState
setRecoveringState model =
    { model | turnState = Recovering }


setDefeatState : RunState -> RunState
setDefeatState model =
    { model | turnState = Defeat }


setVictoryState : List Card -> RunState -> RunState
setVictoryState rewards model =
    { model | turnState = Victory rewards }


getDeadCharacter : RunState -> Maybe Bool
getDeadCharacter model =
    [ ( True, Tuple.first model.characters ), ( False, Tuple.second model.characters ) ]
        |> List.map (\( plr, c ) -> ( plr, Character.isAlive c |> not ))
        |> List.filter (\( _, a ) -> a)
        |> List.head
        |> Maybe.map Tuple.first


advanceTurnState : RunState -> RunState
advanceTurnState model =
    case model.turnState of
        Recovering ->
            case getDeadCharacter model of
                Just isPlayer ->
                    if isPlayer then
                        setDefeatState model

                    else
                        setVictoryState [ Cards.basicCard2, Cards.expensiveCard ] model

                Nothing ->
                    -- case Character.playCardAtIndex 0 (Tuple.second model.characters) of
                    --     ( newCharacter, Just action ) ->
                    --         { model
                    --             | turnState = Attacking False action (Cooldown.new characterAnimationDuration)
                    --             , characters = Tuple.mapSecond (always newCharacter) model.characters
                    --         }
                    --     ( _, Nothing ) ->
                    --         model
                    case ( Character.isReady (Tuple.first model.characters), Character.isReady (Tuple.second model.characters) ) of
                        ( True, _ ) ->
                            { model
                                | turnState = Attacking True (Attack (Character.deriveStat Stat.Attack (Tuple.first model.characters) |> round)) (Cooldown.new characterAnimationDuration)
                                , characters = Tuple.mapFirst Character.resetCooldown model.characters
                            }

                        ( False, True ) ->
                            { model
                                | turnState = Attacking False (Attack (Character.deriveStat Stat.Attack (Tuple.second model.characters) |> round)) (Cooldown.new characterAnimationDuration)
                                , characters = Tuple.mapSecond Character.resetCooldown model.characters
                            }

                        _ ->
                            model

        Attacking isPlayer action cooldown ->
            if Cooldown.isDone cooldown then
                case action of
                    Card.Attack _ ->
                        model
                            |> updateFlag (Character.applyAction action) (not isPlayer)
                            |> setRecoveringState

                    Card.Buff _ ->
                        model
                            |> updateFlag (Character.applyAction action) isPlayer
                            |> setRecoveringState

            else
                model

        -- Hit _ cooldown ->
        --     if Cooldown.isDone cooldown then
        --         setRecoveringState model
        --     else
        --         model
        Defeat ->
            model

        Victory _ ->
            model



-- VIEW


viewCustomMeter : Int -> Int -> Html msg
viewCustomMeter max value =
    Html.div [ Html.Attributes.class "custom-meter" ]
        [ Html.div
            [ Html.Attributes.class "trail"
            , Html.Attributes.style "width" (String.fromFloat (toFloat value / toFloat max * 100) ++ "%")
            ]
            []
        , Html.div
            [ Html.Attributes.class "value"
            , Html.Attributes.style "width" (String.fromFloat (toFloat value / toFloat max * 100) ++ "%")
            ]
            []
        ]


viewCooldown : Cooldown -> Html msg
viewCooldown ( cd, maxCd ) =
    Html.progress
        [ Html.Attributes.value (String.fromFloat cd)
        , Html.Attributes.max (String.fromFloat maxCd)
        ]
        []


viewBuff : Buff -> Html msg
viewBuff buff =
    Html.li [ Html.Attributes.class "buff" ]
        [ Html.text
            (Stat.toString (Tuple.first buff.statModifier)
                ++ " x"
                ++ String.fromFloat (Tuple.second buff.statModifier)
            )
        , viewCooldown buff.duration
        ]


viewStat : ( Stat, Float ) -> Html msg
viewStat ( statType, statValue ) =
    Html.p [ Html.Attributes.class "stat" ] [ Html.text (Stat.toString statType ++ ": " ++ String.fromFloat statValue) ]


viewHealthHistoryItem : ( Int, Int ) -> ( String, Html msg )
viewHealthHistoryItem ( id, delta ) =
    ( "item" ++ String.fromInt id, Html.p [] [ Html.text (String.fromInt delta) ] )



-- viewEnergy : ( Energy, Float ) -> Maybe (Html msg)
-- viewEnergy ( energy, amount ) =
--     if amount > 0 then
--         Just
--             (Html.div [ Html.Attributes.class (Energy.toString energy) ]
--                 [ Html.p [] [ Html.text (String.fromInt (floor amount)) ]
--                 , Html.progress
--                     [ Html.Attributes.value (String.fromFloat (amount - toFloat (floor amount)))
--                     , Html.Attributes.max "1"
--                     ]
--                     []
--                 -- , viewCooldown cooldown
--                 ]
--             )
--     else
--         Nothing


viewCardCost : ( Energy, Int ) -> Html msg
viewCardCost ( energy, amount ) =
    Html.p [ Html.Attributes.class (Energy.toString energy) ] [ Html.text (String.fromInt amount) ]



-- viewSmallCard : Character -> Card -> Html msg
-- viewSmallCard character card =
--     Html.div
--         [ Html.Attributes.class "card"
--         , Html.Attributes.classList [ ( "can-afford", Character.canAfford character card.cost ) ]
--         ]
--         [ Html.div [] (card.cost |> Dict.toList |> List.map viewCardCost)
--         , Html.p [] [ Html.text (Card.actionToIcon card.action) ]
--         ]


characterClasses : TurnState -> Bool -> List (Attribute msg)
characterClasses turnState isPlayer =
    let
        isAttacking : Bool
        isAttacking =
            case turnState of
                Attacking characterType action _ ->
                    case action of
                        Card.Attack _ ->
                            characterType == isPlayer

                        Card.Buff _ ->
                            False

                _ ->
                    False

        isBuffing : Bool
        isBuffing =
            case turnState of
                Attacking characterType action _ ->
                    case action of
                        Card.Buff _ ->
                            characterType == isPlayer

                        Card.Attack _ ->
                            False

                _ ->
                    False

        isHit : Bool
        isHit =
            case turnState of
                Attacking characterType action _ ->
                    case action of
                        Card.Attack _ ->
                            characterType /= isPlayer

                        Card.Buff _ ->
                            False

                _ ->
                    False

        isDead : Bool
        isDead =
            case turnState of
                Defeat ->
                    isPlayer

                _ ->
                    False

        isWinner : Bool
        isWinner =
            case turnState of
                Victory _ ->
                    isPlayer

                _ ->
                    False
    in
    [ Html.Attributes.class (characterTypeString isPlayer)
    , Html.Attributes.classList
        [ ( "attacking", isAttacking )
        , ( "hit", isHit )
        , ( "dead", isDead )
        , ( "winner", isWinner )
        , ( "buffing", isBuffing )
        ]
    ]


viewCharacter : List (Attribute msg) -> Character -> Html msg
viewCharacter attrs character =
    Html.div
        (Html.Attributes.class "character" :: attrs)
        [ Html.h1 [ Html.Attributes.class "icon" ] [ Html.text (String.fromChar character.icon) ]
        , Html.Keyed.node "div" [ Html.Attributes.class "health-history" ] (List.map viewHealthHistoryItem character.healthHistory)

        -- , Html.p []
        --     [ Html.text
        --         ("hlt: "
        --             ++ String.fromInt (Tuple.first character.health)
        --             ++ "/"
        --             ++ String.fromInt (Tuple.second character.health)
        --         )
        --     ]
        , Html.p [] [ Html.text "health" ]
        , viewCustomMeter (Tuple.second character.health) (Tuple.first character.health)

        -- , Html.div [ Html.Attributes.class "energy" ] (Dict.toList character.energy |> List.filterMap viewEnergy)
        -- , Html.div [ Html.Attributes.class "hand" ] (List.map (viewSmallCard character) character.hand)
        , Html.p [] [ Html.text "cooldown" ]
        , viewCooldown character.cooldown

        -- , Html.details [ Html.Attributes.class "stats" ]
        --     [ Html.summary []
        --         [ Html.text "Stats" ]
        --     , Html.div []
        --         (Character.deriveStats character
        --             |> List.map viewStat
        --         )
        --     ]
        , Html.ul [ Html.Attributes.class "buffs" ] (List.map viewBuff character.buffs)
        ]


viewCharacterPreview : List (Attribute msg) -> Character -> Html msg
viewCharacterPreview attrs character =
    Html.div (Html.Attributes.class "character-preview" :: attrs)
        [ Html.h1 [ Html.Attributes.class "icon" ] [ Html.text (String.fromChar character.icon) ]
        , Html.div []
            (Character.deriveStats character
                |> List.map viewStat
            )
        ]


viewCard : List (Attribute msg) -> Card -> Html msg
viewCard attrs card =
    Html.div
        (Html.Attributes.class "card" :: attrs)
        [ Html.h3 [] [ Html.text card.name ]
        , Html.div [ Html.Attributes.class "cost" ] (card.cost |> Dict.toList |> List.map viewCardCost)
        , Html.p [] [ Html.text (Card.actionToString card.action) ]
        ]


viewPlayerDeckStats : Character -> Html msg
viewPlayerDeckStats character =
    Html.div [ Html.Attributes.class "deck-stats" ]
        [ Html.p [] [ Html.text ("Deck: " ++ String.fromInt (List.length character.deck)) ]
        , Html.p [] [ Html.text ("Played: " ++ String.fromInt (List.length character.played)) ]
        ]


viewPlayerHand : Character -> Html Msg
viewPlayerHand character =
    let
        cardAttributes index card =
            [ Html.Attributes.classList [ ( "cant-afford", Character.canAfford character card.cost |> not ) ]
            , Html.Events.onClick (ClickedPlayerCard index)
            ]
    in
    Html.div [ Html.Attributes.class "player-hand" ] (List.indexedMap (\index card -> viewCard (cardAttributes index card) card) character.hand)


viewDefeat : Html Msg
viewDefeat =
    Html.div []
        [ Html.p [] [ Html.text "Defeat :(" ]
        , Html.button [ Html.Events.onClick ClickedResetGame ] [ Html.text "Reset" ]
        ]


viewVictory : List Card -> Html Msg
viewVictory rewards =
    let
        viewReward reward =
            viewCard [ Html.Events.onClick (ClickedReward reward) ] reward
    in
    Html.div []
        [ Html.p [] [ Html.text "Victory!" ]
        , Html.div [ Html.Attributes.class "card-group" ] (List.map viewReward rewards)
        , Html.button [ Html.Events.onClick ClickedNextEnemy ] [ Html.text "Next enemy" ]
        , Html.button [ Html.Events.onClick ClickedReturnHome ] [ Html.text "Return home" ]
        ]


viewEncounters : List Character -> Html msg
viewEncounters encounters =
    Html.div []
        [ Html.h3 [] [ Html.text "Next encounters" ]
        , Html.div [ Html.Attributes.class "encounters" ] (List.map (\character -> Html.p [] [ Html.text (String.fromChar character.icon) ]) encounters)
        ]


viewRun : RunState -> Html Msg
viewRun runState =
    Html.div [ Html.Attributes.class "run" ]
        (case runState.turnState of
            Defeat ->
                [ viewDefeat ]

            Victory rewards ->
                [ viewVictory rewards
                , viewEncounters runState.encounters
                ]

            _ ->
                [ Html.div [ Html.Attributes.class "characters" ]
                    [ viewCharacter (characterClasses runState.turnState True) (Tuple.first runState.characters)
                    , viewCharacter (characterClasses runState.turnState False) (Tuple.second runState.characters)
                    ]
                , viewPlayerDeckStats (Tuple.first runState.characters)
                , viewPlayerHand (Tuple.first runState.characters)
                ]
        )


viewHome : HomeState -> Model -> Html Msg
viewHome homeState model =
    let
        viewCharacterPreset character =
            viewCharacterPreview [ Html.Events.onClick (ClickedCharacterPreset character) ] character
    in
    Html.div [ Html.Attributes.class "home" ]
        [ Html.h1 [] [ Html.text "Home" ]
        , Html.h3 [] [ Html.text "Selected Character" ]
        , Html.div []
            (case homeState.character of
                Just character ->
                    [ viewCharacterPreview [] character
                    , Html.button [ Html.Events.onClick (ClickedStartRun character homeState.deck) ] [ Html.text "Start run" ]
                    ]

                Nothing ->
                    []
            )
        , Html.h3 [] [ Html.text "Selected Cards" ]
        , Html.div [ Html.Attributes.class "card-group" ] (List.indexedMap (\index card -> viewCard [ Html.Events.onClick (ClickedCardInSelectedCards index card) ] card) homeState.deck)
        , Html.h3 [] [ Html.text "Characters" ]
        , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "1rem" ]
            [ viewCharacterPreset Characters.panda
            , viewCharacterPreset Characters.unicorn
            ]
        , Html.h3 [] [ Html.text "Card Collection" ]
        , Html.div [ Html.Attributes.class "card-group" ] (List.indexedMap (\index card -> viewCard [ Html.Events.onClick (ClickedCardInCollection index card) ] card) model.cards)
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        (case model.gameState of
            Run runState ->
                [ viewRun runState ]

            Home homeState ->
                [ viewHome homeState model ]
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Run _ ->
            Browser.Events.onAnimationFrameDelta (min 1000 >> Tick)

        Home _ ->
            Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update =
            \msg model ->
                case msg of
                    ClickedReturnHome ->
                        ( update msg model, Codec.saveCards model.cards )

                    _ ->
                        ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
