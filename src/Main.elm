module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Buff exposing (Buff)
import Card exposing (Action, Card)
import Character exposing (Character)
import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Stat exposing (Stat)



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    300



-- CONTENT


playerCharacter : Character
playerCharacter =
    Character.new
        [ ( Stat.Attack, 10 )
        , ( Stat.CyanRegenModifier, 2 )
        , ( Stat.YellowRegenModifier, 0.7 )
        ]
        100
        |> Character.drawCard basicCard
        |> Character.drawCard expensiveCard
        |> Character.drawCard buffCard


enemyCharacter : Character
enemyCharacter =
    Character.new
        [ ( Stat.Attack, 10 )
        , ( Stat.CyanRegenModifier, 1 )
        ]
        20


basicCard : Card
basicCard =
    Card.new "Basic Attack" (Card.Attack 1) [ ( Energy.Cyan, 2 ) ]


expensiveCard : Card
expensiveCard =
    Card.new "Expensive Attack" (Card.Attack 10) [ ( Energy.Cyan, 8 ), ( Energy.Yellow, 7 ) ]


buffCard : Card
buffCard =
    Card.new "Buff yellow regen"
        (Card.Buff (Buff.new 3000 ( Stat.YellowRegenModifier, 3 )))
        [ ( Energy.Yellow, 3 ) ]



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
    | Hit Bool Cooldown
    | Done Bool



-- MODEL


type alias Model =
    { characters : Dict Bool Character
    , turnState : TurnState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( True, playerCharacter )
            , ( False, enemyCharacter )
            ]
        )
        Recovering
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedResetEnemy
    | ClickedResetGame
    | ClickedPlayerCard Card


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickCharacters dt
                |> tickTurnState dt
                |> advanceTurnState

        ClickedResetEnemy ->
            { model
                | characters = model.characters |> Dict.update False (always enemyCharacter)
                , turnState = Recovering
            }

        ClickedResetGame ->
            init () |> Tuple.first

        ClickedPlayerCard card ->
            case model.turnState of
                Recovering ->
                    playCard card model

                _ ->
                    model


playCard : Card -> Model -> Model
playCard card model =
    case Dict.get True model.characters of
        Just player ->
            if Character.canAfford player card.cost then
                { model
                    | turnState = Attacking True card.action (Cooldown.new characterAnimationDuration)
                    , characters = Dict.update True (Character.removeEnergy card.cost) model.characters
                }

            else
                model

        Nothing ->
            model


tickCharacters : Float -> Model -> Model
tickCharacters dt model =
    case ( model.turnState, Dict.all Character.isAlive model.characters ) of
        ( Recovering, True ) ->
            { model
                | characters =
                    model.characters
                        |> Dict.map (\_ character -> Character.tick dt character)
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

        Hit isPlayer cooldown ->
            { model | turnState = Hit isPlayer (Cooldown.tick dt cooldown) }

        Done _ ->
            model


setHitState : Bool -> Action -> Model -> Model
setHitState isPlayer action model =
    { model
        | turnState = Hit (not isPlayer) (Cooldown.new characterAnimationDuration)
        , characters =
            model.characters
                -- |> Dict.update isPlayer Character.resetCooldown
                |> Dict.update (not isPlayer) (Character.applyAction action)
    }


setRecoveringState : Model -> Model
setRecoveringState model =
    { model | turnState = Recovering }


setDoneState : Bool -> Model -> Model
setDoneState isPlayer model =
    { model | turnState = Done (not isPlayer) }


getDeadCharacter : Model -> Maybe Bool
getDeadCharacter model =
    model.characters
        |> Dict.filter (\_ character -> not (Character.isAlive character))
        |> Dict.toList
        |> List.head
        |> Maybe.map Tuple.first


advanceTurnState : Model -> Model
advanceTurnState model =
    case model.turnState of
        Recovering ->
            case getDeadCharacter model of
                Just isPlayer ->
                    setDoneState isPlayer model

                Nothing ->
                    model

        Attacking isPlayer action cooldown ->
            if Cooldown.isDone cooldown then
                case action of
                    Card.Attack _ ->
                        setHitState isPlayer action model

                    Card.Buff _ ->
                        setHitState (not isPlayer) action model

            else
                model

        Hit _ cooldown ->
            if Cooldown.isDone cooldown then
                setRecoveringState model

            else
                model

        Done _ ->
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
                ++ "x"
                ++ String.fromFloat (Tuple.second buff.statModifier)
            )
        , viewCooldown buff.duration
        ]


viewStat : ( Stat, Float ) -> Html msg
viewStat ( statType, statValue ) =
    Html.p [ Html.Attributes.class "stat" ] [ Html.text (Stat.toString statType ++ ": " ++ String.fromFloat statValue) ]


viewHealthHistoryItem : Int -> Html msg
viewHealthHistoryItem delta =
    Html.p [] [ Html.text (String.fromInt delta) ]


viewEnergy : ( Energy, ( Cooldown, ( Int, Int ) ) ) -> Maybe (Html msg)
viewEnergy ( energy, ( cooldown, ( amount, cap ) ) ) =
    if amount > 0 then
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


viewCharacter : TurnState -> ( Bool, Character ) -> Html msg
viewCharacter turnState ( isPlayer, character ) =
    let
        isAttacking : Bool
        isAttacking =
            case turnState of
                Attacking characterType _ _ ->
                    characterType == isPlayer

                _ ->
                    False

        isHit : Bool
        isHit =
            case turnState of
                Hit characterType _ ->
                    characterType == isPlayer

                _ ->
                    False

        isDead : Bool
        isDead =
            case turnState of
                Done characterType ->
                    characterType /= isPlayer

                _ ->
                    False

        isWinner : Bool
        isWinner =
            case turnState of
                Done characterType ->
                    characterType == isPlayer

                _ ->
                    False
    in
    Html.div
        [ Html.Attributes.class "character"
        , Html.Attributes.class (characterTypeString isPlayer)
        , Html.Attributes.classList
            [ ( "attacking", isAttacking )
            , ( "hit", isHit )
            , ( "dead", isDead )
            , ( "winner", isWinner )
            ]
        ]
        [ Html.h1 [ Html.Attributes.class "icon" ] [ Html.text "🐼" ]
        , Html.div [ Html.Attributes.class "health-history" ] (List.map viewHealthHistoryItem character.healthHistory)
        , Html.p []
            [ Html.text
                ("hlt: "
                    ++ String.fromInt (Tuple.first character.health)
                    ++ "/"
                    ++ String.fromInt (Tuple.second character.health)
                )
            ]
        , viewCustomMeter (Tuple.second character.health) (Tuple.first character.health)
        , Html.div [ Html.Attributes.class "energy" ] (Dict.toList character.energy |> List.filterMap viewEnergy)
        , Html.div [ Html.Attributes.class "hand" ] (List.map (viewSmallCard character) character.hand)
        , Html.details []
            (Html.summary [] [ Html.text "Stats" ]
                :: (Character.deriveStats character
                        |> List.map viewStat
                   )
            )
        , Html.ul [ Html.Attributes.class "buffs" ] (List.map viewBuff character.buffs)
        ]


viewCard : Character -> Card -> Html Msg
viewCard character card =
    Html.div
        [ Html.Attributes.class "card"
        , Html.Attributes.classList [ ( "can-afford", Character.canAfford character card.cost ) ]
        , Html.Events.onClick (ClickedPlayerCard card)
        ]
        [ Html.h3 [] [ Html.text card.name ]
        , Html.div [] (card.cost |> Dict.toList |> List.map viewCardCost)
        , Html.p [] [ Html.text (Card.actionToString card.action) ]
        ]


viewPlayerHand : Character -> Html Msg
viewPlayerHand character =
    Html.div [ Html.Attributes.class "player-hand" ] (List.map (viewCard character) character.hand)


viewCombatSummary : Bool -> Html Msg
viewCombatSummary playerVictory =
    if playerVictory then
        Html.div []
            [ Html.p [] [ Html.text "Victory!" ]
            , Html.button [ Html.Events.onClick ClickedResetEnemy ] [ Html.text "Next enemy" ]
            ]

    else
        Html.div []
            [ Html.p [] [ Html.text "Defeat :(" ]
            , Html.button [ Html.Events.onClick ClickedResetGame ] [ Html.text "Reset" ]
            ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ case model.turnState of
            Done isPlayer ->
                viewCombatSummary isPlayer

            _ ->
                Html.div [ Html.Attributes.class "characters" ]
                    (model.characters
                        |> Dict.toList
                        |> List.map (viewCharacter model.turnState)
                    )
        , Html.div [] ([ Dict.get True model.characters |> Maybe.map viewPlayerHand ] |> List.filterMap identity)
        ]



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
