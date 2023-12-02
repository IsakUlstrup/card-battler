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
    | Reward (List Card)


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


nextEncounter2 : RunState -> RunState
nextEncounter2 runState =
    case List.head runState.encounters of
        Just character ->
            { runState
                | playerMinions = List.map Minion.resetCooldown runState.playerMinions
                , opponentMinions =
                    character
                        :: runState.opponentMinions
                        |> List.filter (.minion >> Minion.isAlive)
                , encounters = List.drop 1 runState.encounters
                , turnState = Recovering
            }

        Nothing ->
            runState


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

        Reward _ ->
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
    { model | turnState = Reward rewards }


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


resetDoneCooldowns : RunState -> RunState
resetDoneCooldowns runState =
    let
        resetIfDone minion =
            if Minion.isReady minion then
                Minion.resetCooldown minion

            else
                minion
    in
    { runState
        | playerMinions = List.map resetIfDone runState.playerMinions
        , opponentMinions = List.map (Opponent.updateMinion resetIfDone) runState.opponentMinions
    }


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
                    if List.isEmpty rewards then
                        nextEncounter2 model

                    else
                        setVictoryState rewards { model | seed = seed }

                _ ->
                    case getReadyMinion model of
                        Just ( True, minion ) ->
                            { model
                                | turnState = Attacking True 0 (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)

                                -- , playerMinions = List.map Minion.resetCooldown model.playerMinions
                            }

                        Just ( False, minion ) ->
                            { model
                                | turnState = Attacking False 0 (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)

                                -- , opponentMinions = List.map (Opponent.updateMinion Minion.resetCooldown) model.opponentMinions
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

        Reward _ ->
            model


returnHome : Model -> Model
returnHome model =
    case model.gameState of
        Run runState ->
            if runState.playerMinions |> List.any Minion.isAlive then
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

        Home ->
            model



-- UPDATE


type Msg
    = Tick Float
    | ClickedNextEnemy
    | ClickedReturnHome
    | ClickedCard Int
    | ClickedReward Card
    | ClickedStartRun
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
                                    |> resetDoneCooldowns
                                )
                      }
                    , Cmd.none
                    )

                Home ->
                    ( model, Cmd.none )

        ClickedNextEnemy ->
            ( nextEncounter model, Cmd.none )

        ClickedReturnHome ->
            let
                newModel : Model
                newModel =
                    returnHome model
            in
            ( newModel
            , Codec.saveCards (List.map Tuple.second newModel.cards)
            )

        ClickedCard index ->
            case model.gameState of
                Run runState ->
                    ( { model | gameState = Run (playCard index runState) }, Cmd.none )

                Home ->
                    ( { model
                        | cards = List.indexedMap (toggleIndex index) model.cards
                      }
                    , Cmd.none
                    )

        ClickedReward card ->
            case model.gameState of
                Run runState ->
                    ( { model
                        | gameState =
                            Run
                                { runState
                                    | turnState = Reward []
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

        ClickedCharacterPreset clickedIndex ->
            ( { model
                | characters = List.indexedMap (setIndex clickedIndex) model.characters
              }
            , Cmd.none
            )



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


characterClasses : Int -> TurnState -> Bool -> List (Attribute msg)
characterClasses minionIndex turnState isAlly =
    let
        isAttacking : Bool
        isAttacking =
            case turnState of
                Attacking teamFlag index _ _ ->
                    minionIndex == index && isAlly == teamFlag

                _ ->
                    False
    in
    if isAlly then
        [ Html.Attributes.classList
            [ ( "text-glow-beige", isAttacking )
            , ( "translate-right", isAttacking )
            ]
        , Html.Attributes.class "transition-transform"
        ]

    else
        [ Html.Attributes.classList
            [ ( "text-glow-beige", isAttacking )
            , ( "translate-left", isAttacking )
            ]
        , Html.Attributes.class "transition-transform"
        ]


viewCharacter : List (Attribute msg) -> Minion -> Html msg
viewCharacter iconAttrs character =
    Html.div
        [ Html.Attributes.class "flex flex-column gap-medium" ]
        [ Html.h1 (Html.Attributes.class "font-big center-text" :: iconAttrs) [ Html.text (String.fromChar character.icon) ]
        , Html.p [] [ Html.text ("health: " ++ String.fromInt character.health) ]
        , Html.p [] [ Html.text "cooldown" ]
        , viewCooldown (Tuple.first character.ability)
        ]


viewMinionPreview : List (Attribute msg) -> Minion -> Html msg
viewMinionPreview attrs minion =
    Html.div (Html.Attributes.class "flex flex-column gap-small pointer padding-medium border border-radius-medium bg-pink" :: attrs)
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
        (Html.Attributes.class "flex flex-column gap-medium padding-medium border border-radius-medium pointer no-select width-medium height-large bg-mint text-grey" :: attrs)
        [ Html.h3 [ Html.Attributes.class "center-text" ] [ Html.text card.name ]
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
            [ Html.Attributes.classList [ ( "semi-transparent", Deck.canAfford deck card.cost |> not ) ]
            , Html.Events.onClick (ClickedCard index)
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
    case runState.turnState of
        Defeat ->
            [ viewDefeat ]

        Reward rewards ->
            [ viewVictory (List.map .minion runState.encounters) rewards
            ]

        _ ->
            [ Html.div [ Html.Attributes.style "width" "100%", Html.Attributes.class "flex space-evenly gap-large" ]
                [ Html.div [ Html.Attributes.class "flex gap-medium" ]
                    (List.indexedMap
                        (\index minion ->
                            viewCharacter (characterClasses index runState.turnState True) minion
                        )
                        (List.reverse runState.playerMinions)
                    )
                , Html.div [ Html.Attributes.class "flex gap-medium" ]
                    (List.indexedMap
                        (\index opponent ->
                            viewCharacter (characterClasses index runState.turnState False) opponent.minion
                        )
                        runState.opponentMinions
                    )
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
                , Html.Attributes.classList [ ( "glow-beige", selected ) ]
                , Html.Attributes.class "width-medium"
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
                    [ Html.Events.onClick (ClickedCard index)
                    , Html.Attributes.classList [ ( "glow-beige", selected ) ]
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
        , Html.Attributes.class "flex flex-column center-cross padding-medium gap-large text-beige helvetica"
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
