module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Buff exposing (Buff)
import Card exposing (Action, Card)
import Character exposing (Character)
import Content.Cards as Cards
import Content.Characters as Characters
import Cooldown exposing (Cooldown)
import CustomDict as Dict
import Energy exposing (Energy)
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events
import Html.Keyed
import Stat



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    300



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
      -- | Hit Bool Cooldown
    | Defeat
    | Victory (List Card)



-- MODEL


type alias Model =
    { characters : ( Character, Character )
    , turnState : TurnState
    , encounters : List Character
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        ( Characters.panda Cards.testDeck1 |> Character.drawHand 5
        , Characters.badger Cards.basicDeck |> Character.drawHand 3
        )
        Recovering
        [ Characters.rabbit [], Characters.chick [] ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedResetEnemy
    | ClickedResetGame
    | ClickedPlayerCard Int
    | ClickedReward Card


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickCharacters dt
                |> tickTurnState dt
                |> advanceTurnState

        ClickedResetEnemy ->
            case List.head model.encounters of
                Just character ->
                    { model
                        | characters =
                            model.characters
                                |> Tuple.mapSecond (always (character |> Character.drawHand 3))
                                |> Tuple.mapFirst Character.resetCards
                                |> Tuple.mapFirst (Character.drawHand 5)
                        , encounters = List.drop 1 model.encounters
                        , turnState = Recovering
                    }

                Nothing ->
                    model

        ClickedResetGame ->
            init () |> Tuple.first

        ClickedPlayerCard index ->
            case model.turnState of
                Recovering ->
                    playCard True index model

                _ ->
                    model

        ClickedReward card ->
            case model.turnState of
                Victory _ ->
                    { model
                        | turnState = Victory []
                        , characters = Tuple.mapFirst (Character.addCard card) model.characters
                    }

                _ ->
                    model


updateFlag : (Character -> Character) -> Bool -> Model -> Model
updateFlag f isPlayer model =
    if isPlayer then
        { model | characters = model.characters |> Tuple.mapFirst f }

    else
        { model | characters = model.characters |> Tuple.mapSecond f }


playCard : Bool -> Int -> Model -> Model
playCard isPlayer index model =
    case Character.playCardAtIndex index (Tuple.first model.characters) of
        ( newCharacter, Just action ) ->
            { model | turnState = Attacking isPlayer action (Cooldown.new characterAnimationDuration) }
                |> updateFlag (always newCharacter) isPlayer

        ( _, Nothing ) ->
            model


tickCharacters : Float -> Model -> Model
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


tickTurnState : Float -> Model -> Model
tickTurnState dt model =
    case model.turnState of
        Recovering ->
            model

        Attacking isPlayer power cooldown ->
            { model | turnState = Attacking isPlayer power (Cooldown.tick dt cooldown) }

        -- Hit isPlayer cooldown ->
        --     { model | turnState = Hit isPlayer (Cooldown.tick dt cooldown) }
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


setRecoveringState : Model -> Model
setRecoveringState model =
    { model | turnState = Recovering }


setDefeatState : Model -> Model
setDefeatState model =
    { model | turnState = Defeat }


setVictoryState : List Card -> Model -> Model
setVictoryState rewards model =
    { model | turnState = Victory rewards }


getDeadCharacter : Model -> Maybe Bool
getDeadCharacter model =
    [ ( True, Tuple.first model.characters ), ( False, Tuple.second model.characters ) ]
        |> List.map (\( plr, c ) -> ( plr, Character.isAlive c |> not ))
        |> List.filter (\( _, a ) -> a)
        |> List.head
        |> Maybe.map Tuple.first


advanceTurnState : Model -> Model
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
                    case Character.playCardAtIndex 0 (Tuple.second model.characters) of
                        ( newCharacter, Just action ) ->
                            { model
                                | turnState = Attacking False action (Cooldown.new characterAnimationDuration)
                                , characters = Tuple.mapSecond (always newCharacter) model.characters
                            }

                        ( _, Nothing ) ->
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



-- viewStat : ( Stat, Float ) -> Html msg
-- viewStat ( statType, statValue ) =
--     Html.p [ Html.Attributes.class "stat" ] [ Html.text (Stat.toString statType ++ ": " ++ String.fromFloat statValue) ]


viewHealthHistoryItem : ( Int, Int ) -> ( String, Html msg )
viewHealthHistoryItem ( id, delta ) =
    ( "item" ++ String.fromInt id, Html.p [] [ Html.text (String.fromInt delta) ] )


viewEnergy : ( Energy, ( Cooldown, ( Int, Int ) ) ) -> Maybe (Html msg)
viewEnergy ( energy, ( cooldown, ( amount, cap ) ) ) =
    if amount > 0 || Tuple.first cooldown > 0 then
        Just
            (Html.div [ Html.Attributes.class (Energy.toString energy) ]
                [ Html.p [] [ Html.text (String.fromInt amount ++ "/" ++ String.fromInt cap) ]
                , viewCooldown cooldown
                ]
            )

    else
        Nothing


viewCardCost : ( Energy, Int ) -> Html msg
viewCardCost ( energy, amount ) =
    Html.p [ Html.Attributes.class (Energy.toString energy) ] [ Html.text (String.fromInt amount) ]


viewSmallCard : Character -> Card -> Html msg
viewSmallCard character card =
    Html.div
        [ Html.Attributes.class "card"
        , Html.Attributes.classList [ ( "can-afford", Character.canAfford character card.cost ) ]
        ]
        [ Html.div [] (card.cost |> Dict.toList |> List.map viewCardCost)
        , Html.p [] [ Html.text (Card.actionToIcon card.action) ]
        ]


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
        , viewCustomMeter (Tuple.second character.health) (Tuple.first character.health)
        , Html.div [ Html.Attributes.class "energy" ] (Dict.toList character.energy |> List.filterMap viewEnergy)
        , Html.div [ Html.Attributes.class "hand" ] (List.map (viewSmallCard character) character.hand)

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


viewCard : Character -> Int -> Card -> Html Msg
viewCard character index card =
    Html.div
        [ Html.Attributes.class "card"
        , Html.Attributes.classList [ ( "can-afford", Character.canAfford character card.cost ) ]
        , Html.Events.onClick (ClickedPlayerCard index)
        ]
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
    Html.div [ Html.Attributes.class "player-hand" ] (List.indexedMap (viewCard character) character.hand)


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
            Html.div [ Html.Events.onClick (ClickedReward reward) ] [ Html.text reward.name ]
    in
    Html.div []
        [ Html.p [] [ Html.text "Victory!" ]
        , Html.div [] (List.map viewReward rewards)
        , Html.button [ Html.Events.onClick ClickedResetEnemy ] [ Html.text "Next enemy" ]
        ]


viewEncounters : List Character -> Html msg
viewEncounters encounters =
    Html.div []
        [ Html.h3 [] [ Html.text "Next encounters" ]
        , Html.div [ Html.Attributes.class "encounters" ] (List.map (\character -> Html.p [] [ Html.text (String.fromChar character.icon) ]) encounters)
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        (case model.turnState of
            Defeat ->
                [ viewDefeat ]

            Victory rewards ->
                [ viewVictory rewards
                , viewEncounters model.encounters
                ]

            _ ->
                [ Html.div [ Html.Attributes.class "characters" ]
                    [ viewCharacter (characterClasses model.turnState True) (Tuple.first model.characters)
                    , viewCharacter (characterClasses model.turnState False) (Tuple.second model.characters)
                    ]
                , viewPlayerDeckStats (Tuple.first model.characters)
                , viewPlayerHand (Tuple.first model.characters)
                ]
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 1000 >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
