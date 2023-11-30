module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Card exposing (Action(..), Card)
import Character exposing (Character)
import Codec
import Content.Cards as Cards
import Content.Characters as Characters
import Cooldown exposing (Cooldown)
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events
import Html.Keyed
import Random exposing (Seed)
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



-- MODEL


type alias Flags =
    { timestamp : Int
    , cards : Maybe String
    }


type alias Model =
    { gameState : GameState
    , cards : List Card
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
                    [ Cards.basicCard, Cards.basicCard ]
    in
    ( Model
        (Home (HomeState Nothing []))
        loadCards
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
    | ClickedStartRun Character (List Card)
    | ClickedCardInCollection Int Card
    | ClickedCardInSelectedCards Int Card
    | ClickedCharacterPreset Character


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
                                    |> tickCharacters dt
                                    |> tickTurnState dt
                                    |> advanceTurnState
                                )
                      }
                    , Cmd.none
                    )

                Home _ ->
                    ( model, Cmd.none )

        ClickedNextEnemy ->
            case model.gameState of
                Run runState ->
                    case List.head runState.encounters of
                        Just character ->
                            ( { model
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
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Home _ ->
                    ( model, Cmd.none )

        ClickedReturnHome ->
            case model.gameState of
                Run runState ->
                    let
                        newModel =
                            if runState.characters |> Tuple.first |> Character.isAlive then
                                { model
                                    | gameState = Home (HomeState Nothing [])
                                    , cards = (runState.characters |> Tuple.first |> Character.resetCards |> .deck) ++ model.cards
                                }

                            else
                                { model
                                    | gameState = Home (HomeState Nothing [])
                                }
                    in
                    ( newModel
                    , Codec.saveCards newModel.cards
                    )

                Home _ ->
                    ( model, Cmd.none )

        ClickedPlayerCard index ->
            case model.gameState of
                Run runState ->
                    ( { model | gameState = Run (playCard True index runState) }, Cmd.none )

                Home _ ->
                    ( model, Cmd.none )

        ClickedReward card ->
            case model.gameState of
                Run runState ->
                    ( { model
                        | gameState =
                            Run
                                { runState
                                    | turnState = Victory []
                                    , characters = Tuple.mapFirst (Character.addCard card) runState.characters
                                }
                      }
                    , Cmd.none
                    )

                Home _ ->
                    ( model, Cmd.none )

        ClickedStartRun player deck ->
            ( { model
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
            , Cmd.none
            )

        ClickedCardInCollection index card ->
            case model.gameState of
                Home homeState ->
                    ( { model
                        | gameState = Home { homeState | deck = card :: homeState.deck }
                        , cards = List.take index model.cards ++ List.drop (index + 1) model.cards
                      }
                    , Cmd.none
                    )

                Run _ ->
                    ( model, Cmd.none )

        ClickedCardInSelectedCards index card ->
            case model.gameState of
                Home homeState ->
                    ( { model
                        | gameState = Home { homeState | deck = List.take index homeState.deck ++ List.drop (index + 1) homeState.deck }
                        , cards = card :: model.cards
                      }
                    , Cmd.none
                    )

                Run _ ->
                    ( model, Cmd.none )

        ClickedCharacterPreset character ->
            case model.gameState of
                Home homeState ->
                    ( { model
                        | gameState = Home { homeState | character = Just character }
                      }
                    , Cmd.none
                    )

                Run _ ->
                    ( model, Cmd.none )


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
            model
                |> updateFlag (always newCharacter) isPlayer
                |> updateFlag (Character.applyAction action) (not isPlayer)

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
                    case ( Character.isReady (Tuple.first model.characters), Character.isReady (Tuple.second model.characters) ) of
                        ( True, _ ) ->
                            { model
                                | turnState = Attacking True (Tuple.first model.characters).ability (Cooldown.new characterAnimationDuration)
                                , characters = Tuple.mapFirst Character.resetCooldown model.characters
                            }

                        ( False, True ) ->
                            { model
                                | turnState = Attacking False (Tuple.second model.characters).ability (Cooldown.new characterAnimationDuration)
                                , characters = Tuple.mapSecond Character.resetCooldown model.characters
                            }

                        _ ->
                            model

        Attacking isPlayer action cooldown ->
            if Cooldown.isDone cooldown then
                case action of
                    Card.Damage _ ->
                        model
                            |> updateFlag (Character.applyAction action) (not isPlayer)
                            |> setRecoveringState

            else
                model

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


viewStat : ( Stat, Float ) -> Html msg
viewStat ( statType, statValue ) =
    Html.p [ Html.Attributes.class "stat" ] [ Html.text (Stat.toString statType ++ ": " ++ String.fromFloat statValue) ]


viewHealthHistoryItem : ( Int, Int ) -> ( String, Html msg )
viewHealthHistoryItem ( id, delta ) =
    ( "item" ++ String.fromInt id, Html.p [] [ Html.text (String.fromInt delta) ] )


viewEnergy : Float -> Maybe (Html msg)
viewEnergy amount =
    if amount > 0 then
        Just
            (Html.div []
                [ Html.p [] [ Html.text (String.fromInt (floor amount)) ]
                , Html.progress
                    [ Html.Attributes.value (String.fromFloat (amount - toFloat (floor amount)))
                    , Html.Attributes.max "1"
                    ]
                    []
                ]
            )

    else
        Nothing


viewCardCost : Int -> Html msg
viewCardCost cost =
    Html.p [] [ Html.text ("Cost: " ++ String.fromInt cost) ]


characterClasses : TurnState -> Bool -> List (Attribute msg)
characterClasses turnState isPlayer =
    let
        isAttacking : Bool
        isAttacking =
            case turnState of
                Attacking characterType action _ ->
                    case action of
                        Card.Damage _ ->
                            characterType == isPlayer

                _ ->
                    False
    in
    [ Html.Attributes.class (characterTypeString isPlayer)
    , Html.Attributes.classList
        [ ( "attacking", isAttacking )
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
        , Html.p [] [ Html.text "energy" ]
        , Html.div [ Html.Attributes.class "energy" ] ([ character.energy ] |> List.filterMap viewEnergy)

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
        -- , Html.ul [ Html.Attributes.class "buffs" ] (List.map viewBuff character.buffs)
        ]


viewCharacterPreview : List (Attribute msg) -> Character -> Html msg
viewCharacterPreview attrs character =
    Html.div (Html.Attributes.class "character-preview" :: attrs)
        [ Html.h1 [ Html.Attributes.class "icon" ] [ Html.text (String.fromChar character.icon) ]
        , Html.p [] [ Html.text ("health: " ++ String.fromInt (Tuple.second character.health)) ]
        , Html.p [] [ Html.text ("ability: " ++ Card.actionToString character.ability) ]
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
        , card.cost |> viewCardCost
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
        , Html.button [ Html.Events.onClick ClickedReturnHome ] [ Html.text "Return home" ]
        ]


viewVictory : List Character -> List Card -> Html Msg
viewVictory encounters rewards =
    let
        viewReward reward =
            viewCard [ Html.Events.onClick (ClickedReward reward) ] reward
    in
    Html.div []
        [ Html.p [] [ Html.text "Victory!" ]
        , Html.div [ Html.Attributes.class "card-group" ] (List.map viewReward rewards)
        , if List.isEmpty encounters then
            Html.button [ Html.Events.onClick ClickedReturnHome ] [ Html.text "Return home" ]

          else
            Html.button [ Html.Events.onClick ClickedNextEnemy ] [ Html.text "Next enemy" ]
        , viewEncounters encounters
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
                [ viewVictory runState.encounters rewards
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
            , viewCharacterPreset Characters.butterfly
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
