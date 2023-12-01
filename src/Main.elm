module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Codec
import Content.Cards as Cards
import Content.Minions as Minions
import Content.Opponents
import Cooldown exposing (Cooldown)
import Deck exposing (Action(..), Card, Deck)
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events
import Minion exposing (Minion)
import Opponent exposing (Opponent)
import Random exposing (Seed)



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    150



-- TURN STATE


type TurnState
    = Recovering
    | Attacking Bool Int Int Cooldown
    | Defeat
    | Victory (List Card)


type alias RunState =
    { playerMinions : List Minion
    , opponentMinions : List Opponent
    , turnState : TurnState
    , encounters : List Opponent
    , deck : Deck
    , seed : Seed
    }


type GameState
    = Run RunState
    | Home



-- MODEL


type alias Flags =
    { timestamp : Int
    , cards : Maybe String
    }


type alias Model =
    { gameState : GameState
    , cards : List ( Bool, Card )
    , characters : List ( Bool, Minion )
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        loadCards =
            case flags.cards of
                Just cardsString ->
                    Codec.decodeStoredCards cardsString

                Nothing ->
                    [ Cards.basicCard, Cards.basicCard, Cards.summonCard ]
    in
    ( Model
        Home
        (loadCards |> List.map (Tuple.pair False))
        ([ Minions.panda, Minions.unicorn, Minions.butterfly ] |> List.map (Tuple.pair False))
        (Random.initialSeed flags.timestamp)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedNextEnemy
    | ClickedReturnHome
    | ClickedPlayerCard Int
    | ClickedReward Card
    | ClickedStartRun
    | ClickedCardInCollection Int
    | ClickedCharacterPreset Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            case model.gameState of
                Run runState ->
                    ( { model
                        | gameState =
                            Run
                                (runState
                                    |> tickMinions dt
                                    |> tickDeck dt
                                    |> tickTurnState dt
                                    |> advanceTurnState
                                    |> filterDeadMinions
                                )
                      }
                    , Cmd.none
                    )

                Home ->
                    ( model, Cmd.none )

        ClickedNextEnemy ->
            -- case model.gameState of
            --     Run runState ->
            --         case List.head runState.encounters of
            --             Just character ->
            --                 ( { model
            --                     | gameState =
            --                         Run
            --                             { runState
            --                                 | characters =
            --                                     runState.characters
            --                                         |> Tuple.mapSecond (always (character |> Character.drawHand 3))
            --                                         |> Tuple.mapFirst Character.resetCards
            --                                         |> Tuple.mapFirst (Character.drawHand 5)
            --                                 , encounters = List.drop 1 runState.encounters
            --                                 , turnState = Recovering
            --                             }
            --                   }
            --                 , Cmd.none
            --                 )
            --             Nothing ->
            --                 ( model, Cmd.none )
            --     Home ->
            ( nextEncounter model, Cmd.none )

        ClickedReturnHome ->
            case model.gameState of
                Run runState ->
                    let
                        newModel =
                            if runState.playerMinions |> List.all Minion.isAlive then
                                { model
                                    | gameState = Home
                                    , cards =
                                        (runState.deck
                                            |> Deck.resetCards
                                            |> .cards
                                            |> List.map (Tuple.pair False)
                                        )
                                            ++ model.cards
                                    , seed = runState.seed
                                }

                            else
                                { model
                                    | gameState = Home
                                    , seed = runState.seed
                                }
                    in
                    ( newModel
                    , Codec.saveCards (List.map Tuple.second newModel.cards)
                    )

                Home ->
                    ( model, Cmd.none )

        ClickedPlayerCard index ->
            case model.gameState of
                Run runState ->
                    ( { model | gameState = Run (playCard index runState) }, Cmd.none )

                Home ->
                    ( model, Cmd.none )

        ClickedReward card ->
            case model.gameState of
                Run runState ->
                    ( { model
                        | gameState =
                            Run
                                { runState
                                    | turnState = Victory []
                                    , deck = Deck.addCard card runState.deck
                                }
                      }
                        |> nextEncounter
                    , Cmd.none
                    )

                Home ->
                    ( model, Cmd.none )

        ClickedStartRun ->
            let
                player =
                    model.characters |> List.filter Tuple.first |> List.map Tuple.second |> List.head

                deck =
                    model.cards |> List.filter Tuple.first |> List.map Tuple.second |> Deck.new
            in
            case player of
                Just p ->
                    ( { model
                        | gameState =
                            Run
                                (RunState
                                    [ p ]
                                    [ Content.Opponents.badger ]
                                    Recovering
                                    [ Content.Opponents.rabbit
                                    , Content.Opponents.chick
                                    , Content.Opponents.badger
                                    , Content.Opponents.badger
                                    ]
                                    (deck |> Deck.drawHand 5)
                                    model.seed
                                )
                        , cards = model.cards |> List.filter (Tuple.first >> not)
                      }
                        |> resetSelection
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ClickedCardInCollection clickedIndex ->
            ( { model
                | cards = List.indexedMap (toggleIndex clickedIndex) model.cards
              }
            , Cmd.none
            )

        ClickedCharacterPreset clickedIndex ->
            ( { model
                | characters = List.indexedMap (setIndex clickedIndex) model.characters
              }
            , Cmd.none
            )


toggleIndex : Int -> Int -> ( Bool, a ) -> ( Bool, a )
toggleIndex targteIndex index ( selected, item ) =
    if index == targteIndex then
        ( not selected, item )

    else
        ( selected, item )


setIndex : Int -> Int -> ( Bool, a ) -> ( Bool, a )
setIndex targteIndex index ( selected, item ) =
    if index == targteIndex && selected == False then
        ( True, item )

    else
        ( False, item )


nextEncounter : Model -> Model
nextEncounter model =
    case model.gameState of
        Run runState ->
            case List.head runState.encounters of
                Just character ->
                    { model
                        | gameState =
                            Run
                                { runState
                                    | playerMinions = List.map Minion.resetCooldown runState.playerMinions
                                    , opponentMinions =
                                        -- runState.characters
                                        --     |> Tuple.mapSecond (always (character |> Character.drawHand 3))
                                        --     |> Tuple.mapFirst Character.resetCards
                                        --     |> Tuple.mapFirst (Character.drawHand 5)
                                        character
                                            :: runState.opponentMinions
                                            |> List.filter (.minion >> Minion.isAlive)
                                    , encounters = List.drop 1 runState.encounters
                                    , turnState = Recovering
                                }
                    }

                Nothing ->
                    model

        Home ->
            model


resetSelection : Model -> Model
resetSelection model =
    { model
        | cards = model.cards |> List.map (Tuple.mapFirst (always False))
        , characters = model.characters |> List.map (Tuple.mapFirst (always False))
    }



-- updateFlag : (Character -> Character) -> Bool -> RunState -> RunState
-- updateFlag f isPlayer model =
--     if isPlayer then
--         { model | characters = model.characters |> Tuple.mapFirst f }
--     else
--         { model | characters = model.characters |> Tuple.mapSecond f }


playCard : Int -> RunState -> RunState
playCard index model =
    case Deck.playCardAtIndex index model.deck of
        ( newDeck, Just (Damage dmg) ) ->
            { model
                | deck = newDeck
                , opponentMinions = List.map (Opponent.updateMinion (Minion.damage dmg)) (List.take 1 model.opponentMinions) ++ List.drop 1 model.opponentMinions
            }

        ( newDeck, Just (Summon minion) ) ->
            { model
                | deck = newDeck
                , playerMinions = minion :: model.playerMinions
            }

        ( _, Nothing ) ->
            model


tickMinions : Float -> RunState -> RunState
tickMinions dt model =
    case model.turnState of
        Recovering ->
            { model
                | playerMinions = List.map (Minion.tick dt) model.playerMinions
                , opponentMinions = List.map (Opponent.updateMinion (Minion.tick dt)) model.opponentMinions
            }

        _ ->
            model


tickDeck : Float -> RunState -> RunState
tickDeck dt runState =
    { runState | deck = Deck.tickEnergy dt runState.deck }


tickTurnState : Float -> RunState -> RunState
tickTurnState dt model =
    case model.turnState of
        Recovering ->
            model

        Attacking isPlayer index action cooldown ->
            { model | turnState = Attacking isPlayer index action (Cooldown.tick dt cooldown) }

        Defeat ->
            model

        Victory _ ->
            model


filterDeadMinions : RunState -> RunState
filterDeadMinions runState =
    { runState | playerMinions = List.filter Minion.isAlive runState.playerMinions }


setRecoveringState : RunState -> RunState
setRecoveringState model =
    { model | turnState = Recovering }


setDefeatState : RunState -> RunState
setDefeatState model =
    { model | turnState = Defeat }


setVictoryState : List Card -> RunState -> RunState
setVictoryState rewards model =
    { model | turnState = Victory rewards }


getDeadOpponent : RunState -> Maybe Opponent
getDeadOpponent model =
    model.opponentMinions
        |> List.filter (Opponent.filterMinion (Minion.isAlive >> not))
        |> List.head


playerWipe : RunState -> Bool
playerWipe model =
    model.playerMinions
        |> List.all (Minion.isAlive >> not)


getReadyMinion : RunState -> Maybe ( Bool, Minion )
getReadyMinion runState =
    (runState.playerMinions |> List.map (Tuple.pair True))
        ++ (runState.opponentMinions |> List.map .minion |> List.map (Tuple.pair False))
        |> List.filter (\( _, minion ) -> Minion.isReady minion)
        |> List.head


advanceTurnState : RunState -> RunState
advanceTurnState model =
    case model.turnState of
        Recovering ->
            case ( playerWipe model, getDeadOpponent model ) of
                ( True, _ ) ->
                    setDefeatState model

                ( _, Just opponent ) ->
                    let
                        ( rewards, seed ) =
                            Random.step (Opponent.generateLoot opponent) model.seed
                    in
                    setVictoryState rewards { model | seed = seed }

                _ ->
                    case getReadyMinion model of
                        Just ( True, minion ) ->
                            { model
                                | turnState = Attacking True 0 (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)
                                , playerMinions = List.map Minion.resetCooldown model.playerMinions
                            }

                        Just ( False, minion ) ->
                            { model
                                | turnState = Attacking False 0 (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)
                                , opponentMinions = List.map (Opponent.updateMinion Minion.resetCooldown) model.opponentMinions
                            }

                        _ ->
                            model

        Attacking isPlayer _ attack cooldown ->
            if Cooldown.isDone cooldown then
                (if isPlayer then
                    { model | opponentMinions = List.map (Opponent.updateMinion (Minion.damage attack)) (List.take 1 model.opponentMinions) ++ List.drop 1 model.opponentMinions }

                 else
                    { model | playerMinions = List.map (Minion.damage attack) (List.take 1 model.playerMinions) ++ List.drop 1 model.playerMinions }
                )
                    |> setRecoveringState

            else
                model

        Defeat ->
            model

        Victory _ ->
            model



-- VIEW
-- viewCustomMeter : Int -> Int -> Html msg
-- viewCustomMeter max value =
--     Html.div [ Html.Attributes.class "custom-meter" ]
--         [ Html.div
--             [ Html.Attributes.class "trail"
--             , Html.Attributes.style "width" (String.fromFloat (toFloat value / toFloat max * 100) ++ "%")
--             ]
--             []
--         , Html.div
--             [ Html.Attributes.class "value"
--             , Html.Attributes.style "width" (String.fromFloat (toFloat value / toFloat max * 100) ++ "%")
--             ]
--             []
--         ]


viewCooldown : Cooldown -> Html msg
viewCooldown ( cd, maxCd ) =
    Html.progress
        [ Html.Attributes.value (String.fromFloat cd)
        , Html.Attributes.max (String.fromFloat maxCd)
        ]
        []



-- viewBuff : Buff -> Html msg
-- viewBuff buff =
--     Html.li [ Html.Attributes.class "buff" ]
--         [ Html.text
--             (Stat.toString (Tuple.first buff.statModifier)
--                 ++ " x"
--                 ++ String.fromFloat (Tuple.second buff.statModifier)
--             )
--         , viewCooldown buff.duration
--         ]
-- viewStat : ( Stat, Float ) -> Html msg
-- viewStat ( statType, statValue ) =
--     Html.tr []
--         [ Html.td [] [ Html.text (Stat.toString statType) ]
--         , Html.td [] [ Html.text (String.fromFloat statValue) ]
--         ]
-- viewHealthHistoryItem : ( Int, Int ) -> ( String, Html msg )
-- viewHealthHistoryItem ( id, delta ) =
--     ( "item" ++ String.fromInt id, Html.p [] [ Html.text (String.fromInt delta) ] )


viewEnergy : Float -> Html msg
viewEnergy amount =
    Html.div []
        [ Html.p [] [ Html.text (String.fromInt (floor amount)) ]
        , Html.progress
            [ Html.Attributes.value (String.fromFloat (amount - toFloat (floor amount)))
            , Html.Attributes.max "1"
            ]
            []
        ]


viewCardCost : Int -> Html msg
viewCardCost cost =
    Html.p [] [ Html.text ("Cost: " ++ String.fromInt cost) ]



-- characterClasses : TurnState -> Bool -> List (Attribute msg)
-- characterClasses turnState isPlayer =
--     let
--         isAttacking : Bool
--         isAttacking =
--             case turnState of
--                 Attacking characterType index action _ ->
--                     case action of
--                         Card.Damage _ ->
--                             characterType == isPlayer
--                 _ ->
--                     False
--     in
--     [ Html.Attributes.class (characterTypeString isPlayer)
--     , Html.Attributes.classList
--         [ ( "attacking", isAttacking )
--         ]
--     ]


viewCharacter : List (Attribute msg) -> Minion -> Html msg
viewCharacter attrs character =
    Html.div
        (Html.Attributes.class "flex flex-column gap-medium" :: attrs)
        [ Html.h1 [ Html.Attributes.class "font-big center-text" ] [ Html.text (String.fromChar character.icon) ]

        -- , Html.Keyed.node "div" [ Html.Attributes.class "health-history" ] (List.map viewHealthHistoryItem character.healthHistory)
        , Html.p [] [ Html.text ("health: " ++ String.fromInt character.health) ]

        -- , Html.p []
        --     [ Html.text
        --         ("hlt: "
        --             ++ String.fromInt (Tuple.first character.health)
        --             ++ "/"
        --             ++ String.fromInt (Tuple.second character.health)
        --         )
        --     ]
        -- , Html.meter
        --     [ Html.Attributes.value (Tuple.first character.health |> String.fromInt)
        --     , Html.Attributes.max (Tuple.second character.health |> String.fromInt)
        --     ]
        --     []
        -- , viewCustomMeter (Tuple.second character.health) (Tuple.first character.health)
        -- , Html.p [] [ Html.text "energy" ]
        -- , Html.div [ Html.Attributes.class "energy" ] ([ character.energy ] |> List.filterMap viewEnergy)
        -- , Html.div [ Html.Attributes.class "hand" ] (List.map (viewSmallCard character) character.hand)
        , Html.p [] [ Html.text "cooldown" ]
        , viewCooldown (Tuple.first character.ability)

        -- , Html.details [ Html.Attributes.class "stats" ]
        --     [ Html.summary []
        --         [ Html.text "Stats" ]
        --     , Html.div []
        --         (Character.deriveStats character
        --             |> List.map viewStat
        --         )
        --     ]
        -- , Html.ul [ Html.Attributes.class "buffs" ] (List.map viewBuff character.buffs)
        ]


viewMinionPreview : List (Attribute msg) -> Minion -> Html msg
viewMinionPreview attrs minion =
    Html.div (Html.Attributes.class "flex flex-column gap-small pointer padding-medium border border-radius-medium" :: attrs)
        [ Html.h1 [ Html.Attributes.class "center-text font-big no-select" ] [ Html.text (String.fromChar minion.icon) ]
        , Html.p [] [ Html.text ("health: " ++ String.fromInt minion.health) ]
        , Html.p [] [ Html.text ("attack: " ++ String.fromInt (Tuple.second minion.ability)) ]
        , Html.p [] [ Html.text ("speed: " ++ String.fromInt minion.speed) ]

        -- , Html.table []
        --     -- (Character.deriveStats character
        --     --     |> List.map viewStat
        --     -- )
        --     [ Html.tr []
        --         [ Html.td [] [ Html.text "Speed" ]
        --         , Html.td [] [ Html.text (String.fromInt minion.speed) ]
        --         ]
        --     ]
        ]


viewCard : List (Attribute msg) -> Card -> Html msg
viewCard attrs card =
    Html.div
        (Html.Attributes.class "flex flex-column gap-medium padding-medium border border-radius-medium pointer no-select" :: attrs)
        [ Html.h3 [] [ Html.text card.name ]
        , card.cost |> viewCardCost
        , Html.p [] [ Html.text (Deck.actionToString card.action) ]
        ]


viewDeckStatus : Deck -> Html msg
viewDeckStatus deck =
    Html.div [ Html.Attributes.class "flex gap-medium" ]
        [ Html.p [] [ Html.text ("Deck: " ++ String.fromInt (List.length deck.cards)) ]
        , Html.p [] [ Html.text ("Played: " ++ String.fromInt (List.length deck.played)) ]
        , viewEnergy deck.energy
        ]


viewDeckHand : Deck -> Html Msg
viewDeckHand deck =
    let
        cardAttributes index card =
            [ Html.Attributes.classList [ ( "cant-afford", Deck.canAfford deck card.cost |> not ) ]
            , Html.Events.onClick (ClickedPlayerCard index)
            ]
    in
    Html.div [ Html.Attributes.class "flex gap-medium" ] (List.indexedMap (\index card -> viewCard (cardAttributes index card) card) deck.hand)


viewDefeat : Html Msg
viewDefeat =
    Html.div []
        [ Html.p [] [ Html.text "Defeat :(" ]
        , Html.button [ Html.Events.onClick ClickedReturnHome ] [ Html.text "Return home" ]
        ]


viewVictory : List Minion -> List Card -> Html Msg
viewVictory encounters rewards =
    let
        viewReward reward =
            viewCard [ Html.Events.onClick (ClickedReward reward) ] reward
    in
    Html.div [ Html.Attributes.class "flex flex-column gap-large" ]
        [ Html.h1 [ Html.Attributes.class "center-text" ] [ Html.text "Victory!" ]
        , Html.div [ Html.Attributes.class "flex gap-medium" ] (List.map viewReward rewards)
        , if List.isEmpty encounters then
            Html.button [ Html.Events.onClick ClickedReturnHome, Html.Attributes.class "padding-small" ] [ Html.text "Return home" ]

          else
            Html.button [ Html.Events.onClick ClickedNextEnemy, Html.Attributes.class "padding-small" ] [ Html.text "Skip reward" ]
        , viewEncounters encounters
        ]


viewEncounters : List Minion -> Html msg
viewEncounters encounters =
    Html.div []
        [ Html.h3 [] [ Html.text "Next encounters" ]
        , Html.div [ Html.Attributes.class "flex gap-medium" ] (List.map (\character -> Html.p [ Html.Attributes.class "font-big" ] [ Html.text (String.fromChar character.icon) ]) encounters)
        ]


viewRun : RunState -> List (Html Msg)
viewRun runState =
    -- Html.section [ Html.Attributes.class "run" ]
    case runState.turnState of
        Defeat ->
            [ viewDefeat ]

        Victory rewards ->
            [ viewVictory (List.map .minion runState.encounters) rewards
            ]

        _ ->
            [ Html.div [ Html.Attributes.style "width" "100%", Html.Attributes.class "flex space-evenly gap-large" ]
                [ Html.div [ Html.Attributes.class "flex gap-medium" ] (List.map (viewCharacter []) (List.reverse runState.playerMinions))
                , Html.div [ Html.Attributes.class "flex gap-medium" ] (List.map (.minion >> viewCharacter []) runState.opponentMinions)
                ]
            , viewDeckStatus runState.deck
            , viewDeckHand runState.deck

            -- , viewPlayerHand (Tuple.first runState.characters)
            ]


viewHome : Model -> List (Html Msg)
viewHome model =
    let
        viewMinionPreset : Int -> ( Bool, Minion ) -> Html Msg
        viewMinionPreset index ( selected, minion ) =
            viewMinionPreview
                [ Html.Events.onClick (ClickedCharacterPreset index)
                , Html.Attributes.classList [ ( "border-green", selected ) ]
                , Html.Attributes.style "width" "12rem"
                ]
                minion
    in
    [ Html.h1 [] [ Html.text "Home" ]
    , Html.button [ Html.Events.onClick ClickedStartRun, Html.Attributes.class "padding-small" ] [ Html.text "Start run" ]
    , Html.h3 [] [ Html.text "Characters" ]
    , Html.div [ Html.Attributes.class "flex gap-medium" ]
        (List.indexedMap viewMinionPreset model.characters)
    , Html.h3 [] [ Html.text "Card Collection" ]
    , Html.div [ Html.Attributes.class "flex flex-wrap gap-medium" ]
        (List.indexedMap
            (\index ( selected, card ) ->
                viewCard
                    [ Html.Events.onClick (ClickedCardInCollection index)
                    , Html.Attributes.classList [ ( "border-green", selected ) ]
                    , Html.Attributes.class "padding-medium"
                    ]
                    card
            )
            model.cards
        )
    ]


view : Model -> Html Msg
view model =
    main_
        [ Html.Attributes.id "app"
        , Html.Attributes.class "flex flex-column center-cross padding-medium gap-large beige-text helvetica"
        ]
        (case model.gameState of
            Run runState ->
                viewRun runState

            Home ->
                viewHome model
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Run _ ->
            Browser.Events.onAnimationFrameDelta (min 1000 >> Tick)

        Home ->
            Sub.none



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
