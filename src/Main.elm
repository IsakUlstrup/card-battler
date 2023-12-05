module Main exposing (Model, Msg, main)

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
import Random exposing (Seed)
import Run exposing (Run)


type GameState
    = Run Run
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
                                    , turnState = Run.Recovering
                                }
                    }

                Nothing ->
                    { model | gameState = Run { runState | turnState = Run.Recovering, opponentMinions = [] } }

        Home ->
            model


resetSelection : Model -> Model
resetSelection model =
    { model
        | cards = model.cards |> List.map (Tuple.mapFirst (always False))
        , characters = model.characters |> List.map (Tuple.mapFirst (always False))
    }


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
                                    |> Run.tickMinions dt
                                    |> Run.tickDeck dt
                                    |> Run.tickTurnState dt
                                    |> Run.advanceTurnState
                                    |> Run.filterDeadMinions
                                 -- |> Run.resetDoneCooldowns
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
                    ( { model | gameState = Run (Run.playCard index runState) }, Cmd.none )

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
                                    | turnState = Run.Reward []
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
                                (Run.Run
                                    [ p ]
                                    [ Content.Opponents.badger
                                    , Content.Opponents.rabbit
                                    , Content.Opponents.chick
                                    , Content.Opponents.badger
                                    ]
                                    Run.Recovering
                                    [ Content.Opponents.rabbit
                                    , Content.Opponents.chick
                                    , Content.Opponents.badger
                                    , Content.Opponents.badger
                                    , Content.Opponents.chick
                                    , Content.Opponents.badger
                                    , Content.Opponents.badger
                                    , Content.Opponents.chick
                                    , Content.Opponents.badger
                                    , Content.Opponents.badger
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


viewCooldownCircle : Cooldown -> Html msg
viewCooldownCircle ( cd, maxCd ) =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "background" "rgba(255, 255, 255, 0.2)"
        , Html.Attributes.style "transform" ("scale(" ++ String.fromFloat (cd / maxCd) ++ ")")
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "aspect-ratio" "1/1"
        , Html.Attributes.style "border-radius" "999rem"
        , Html.Attributes.style "z-index" "-1"
        , Html.Attributes.style "transition" "transform 300ms"
        ]
        []


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


characterClasses : Minion -> Int -> Run.TurnState -> Bool -> List (Attribute msg)
characterClasses minion minionIndex turnState isAlly =
    let
        isAttacking : Bool
        isAttacking =
            case turnState of
                Run.PlayerAttacking index _ _ ->
                    minionIndex == index && isAlly

                Run.OpponentAttacking index _ _ ->
                    minionIndex == index && not isAlly

                _ ->
                    False
    in
    if isAlly then
        [ Html.Attributes.classList
            [ ( "text-glow-beige", isAttacking )
            , ( "translate-right", isAttacking )
            , ( "rise-fade", minion |> Minion.isAlive |> not )
            ]
        , Html.Attributes.class "transition-transform"
        ]

    else
        [ Html.Attributes.classList
            [ ( "text-glow-beige", isAttacking )
            , ( "translate-left", isAttacking )
            , ( "rise-fade", minion |> Minion.isAlive |> not )
            ]
        , Html.Attributes.class "transition-transform"
        ]


viewCharacter : List (Attribute msg) -> Minion -> Html msg
viewCharacter iconAttrs minion =
    Html.div
        [ Html.Attributes.class "flex flex-column center-cross gap-medium" ]
        [ Html.div [ Html.Attributes.style "position" "relative", Html.Attributes.class "padding-small" ]
            [ viewCooldownCircle (Tuple.first minion.ability)
            , Html.h1 (Html.Attributes.class "center-text" :: iconAttrs) [ Html.text (String.fromChar minion.icon) ]
            ]
        , Html.div [ Html.Attributes.class "flex gap-small" ]
            [ Html.p [ Html.Attributes.class "pos-relative heart" ] [ Html.text (String.fromInt minion.health) ]
            , Html.p [ Html.Attributes.class "pos-relative sword" ] [ Html.text (String.fromInt (Tuple.second minion.ability)) ]
            ]
        ]


viewMinionPreview : List (Attribute msg) -> Minion -> Html msg
viewMinionPreview attrs minion =
    Html.div (Html.Attributes.class "flex flex-column gap-small pointer padding-medium" :: attrs)
        [ Html.h1 [ Html.Attributes.class "center-text font-big no-select" ] [ Html.text (String.fromChar minion.icon) ]
        , Html.div [ Html.Attributes.class "flex gap-medium" ]
            [ Html.p [ Html.Attributes.class "pos-relative heart" ] [ Html.text (String.fromInt minion.health) ]
            , Html.p [ Html.Attributes.class "pos-relative sword" ] [ Html.text (String.fromInt (Tuple.second minion.ability)) ]
            , Html.p [ Html.Attributes.class "pos-relative lightning" ] [ Html.text (String.fromInt minion.speed) ]
            ]
        ]


viewCard : List (Attribute msg) -> Card -> Html msg
viewCard attrs card =
    Html.div (Html.Attributes.class "flex strech-cross gap-medium bg-pink border-radius-medium padding-small pointer no-select text-grey" :: attrs)
        [ Html.h1 [ Html.Attributes.class "font-big" ] [ Html.text (String.fromChar card.icon) ]
        , Html.div
            [ Html.Attributes.class "flex flex-column flex-auto gap-small padding-small border-radius-small bg-beige-transparent bg-blur" ]
            [ Html.h3 [ Html.Attributes.class "center-text" ] [ Html.text card.name ]
            , card.cost |> viewCardCost
            , Html.p [] [ Html.text (Deck.actionToString card.action) ]
            ]
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
    Html.div [ Html.Attributes.class "flex flex-column gap-medium" ] (List.indexedMap (\index card -> viewCard (cardAttributes index card) card) deck.hand)


viewDefeat : Html Msg
viewDefeat =
    Html.div []
        [ Html.p [] [ Html.text "Defeat :(" ]
        , Html.button [ Html.Events.onClick ClickedReturnHome ] [ Html.text "Return home" ]
        ]


viewVictory : Html Msg
viewVictory =
    Html.div [ Html.Attributes.class "flex flex-column gap-large" ]
        [ Html.h1 [ Html.Attributes.class "center-text" ] [ Html.text "Victory!" ]
        , Html.button [ Html.Events.onClick ClickedReturnHome ] [ Html.text "Return home" ]
        ]


viewRewards : Run -> Html Msg
viewRewards run =
    let
        viewReward reward =
            viewCard [ Html.Events.onClick (ClickedReward reward) ] reward
    in
    case run.turnState of
        Run.Reward rewards ->
            Html.div [ Html.Attributes.class "flex flex-column gap-large modal border-radius-medium padding-medium" ]
                [ Html.h1 [ Html.Attributes.class "center-text" ] [ Html.text "Pick a card" ]
                , Html.div [ Html.Attributes.class "flex flex-column gap-medium" ] (List.map viewReward rewards)
                ]

        _ ->
            Html.div [] []


viewRun : Run -> List (Html Msg)
viewRun runState =
    if List.isEmpty runState.opponentMinions && List.isEmpty runState.encounters then
        [ viewVictory ]

    else if Run.playerWipe runState then
        [ viewDefeat ]

    else
        [ Html.div [ Html.Attributes.style "width" "100%", Html.Attributes.class "flex space-evenly gap-large" ]
            [ Html.div [ Html.Attributes.class "flex gap-medium" ]
                (List.indexedMap
                    (\index minion ->
                        viewCharacter (characterClasses minion index runState.turnState True) minion
                    )
                    runState.playerMinions
                )
            , Html.div [ Html.Attributes.class "flex gap-medium" ]
                (List.indexedMap
                    (\index opponent ->
                        viewCharacter (characterClasses opponent.minion index runState.turnState False) opponent.minion
                    )
                    runState.opponentMinions
                )
            ]
        , viewDeckStatus runState.deck
        , viewDeckHand runState.deck
        , viewRewards runState
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
    , Html.div [ Html.Attributes.class "grid-3 gap-large" ]
        (List.indexedMap
            (\index ( selected, card ) ->
                viewCard
                    [ Html.Events.onClick (ClickedCard index)
                    , Html.Attributes.classList [ ( "glow-beige", selected ) ]
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
        , Html.Attributes.class "flex flex-column padding-medium gap-large text-beige helvetica"
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
