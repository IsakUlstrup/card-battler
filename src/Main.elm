module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Card exposing (Card)
import Character exposing (Buff, Character, Stat)
import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    300



-- ATTACK


type alias Attack =
    { name : String
    , cost : Dict Energy Int
    , power : Int
    }



-- CONTENT


playerCharacter : Character
playerCharacter =
    Character.new
        [ ( Character.Attack, 10 )
        , ( Character.CyanRegenModifier, 2 )
        , ( Character.YellowRegenModifier, 0.7 )
        ]
        100
        |> Character.drawCard basicCard


enemyCharacter : Character
enemyCharacter =
    Character.new
        [ ( Character.Attack, 10 )
        , ( Character.CyanRegenModifier, 1 )
        ]
        20


basicCard : Card
basicCard =
    Card.new "Basic Attack" (Card.Attack 1) [ ( Energy.Cyan, 2 ) ]



-- TURN STATE


characterTypeString : Bool -> String
characterTypeString isPlayer =
    if isPlayer then
        "player"

    else
        "enemy"


type TurnState
    = Recovering
    | Attacking Bool Int Cooldown
    | Hit Bool Cooldown
    | Done Bool


turnStateString : TurnState -> String
turnStateString turnState =
    case turnState of
        Recovering ->
            "recovering"

        Attacking _ _ _ ->
            "attacking"

        Hit _ _ ->
            "hit"

        Done _ ->
            "done"



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
    | ClickedSetPlayerAttack Attack


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

        ClickedSetPlayerAttack attack ->
            case model.turnState of
                Recovering ->
                    { model
                        | turnState = Attacking True attack.power (Cooldown.new characterAnimationDuration)
                    }

                _ ->
                    model


tickCharacters : Float -> Model -> Model
tickCharacters dt model =
    case ( model.turnState, Dict.all Character.isAlive model.characters ) of
        ( Recovering, True ) ->
            { model
                | characters =
                    model.characters
                        |> Dict.map (\_ character -> Character.tick dt character)

                -- |> Dict.map (\_ character -> Character.tickBuffs dt character)
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


getHead : (Character -> Bool) -> Model -> Maybe ( Bool, Character )
getHead f model =
    model.characters
        |> Dict.filter (\_ character -> f character)
        |> Dict.toList
        |> List.head


setAttackingState : Bool -> Character -> Model -> Model
setAttackingState isPlayer character model =
    { model
        | turnState = Attacking isPlayer (round (Character.deriveAttack character)) (Cooldown.new characterAnimationDuration)
    }


setHitState : Bool -> Int -> Model -> Model
setHitState isPlayer power model =
    { model
        | turnState = Hit (not isPlayer) (Cooldown.new characterAnimationDuration)
        , characters =
            model.characters
                -- |> Dict.update isPlayer Character.resetCooldown
                |> Dict.update (not isPlayer) (Character.hit power)
    }


setRecoveringState : Model -> Model
setRecoveringState model =
    { model | turnState = Recovering }


setDoneState : Bool -> Model -> Model
setDoneState isPlayer model =
    { model | turnState = Done (not isPlayer) }


advanceTurnState : Model -> Model
advanceTurnState model =
    case model.turnState of
        Recovering ->
            -- case ( getHead (Character.isAlive >> not) model, getHead Character.isReady model ) of
            --     ( Nothing, Just ( isPlayer, character ) ) ->
            --         setAttackingState isPlayer character model
            --     ( Just ( isPlayer, _ ), _ ) ->
            --         setDoneState isPlayer model
            --     _ ->
            model

        Attacking isPlayer power cooldown ->
            if Cooldown.isDone cooldown then
                setHitState isPlayer power model

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
            (Character.statString (Tuple.first buff.statModifier)
                ++ "x"
                ++ String.fromFloat (Tuple.second buff.statModifier)
            )
        , viewCooldown buff.duration
        ]


viewStat : ( Stat, Float ) -> Html msg
viewStat ( statType, statValue ) =
    Html.p [ Html.Attributes.class "stat" ] [ Html.text (Character.statString statType ++ ": " ++ String.fromFloat statValue) ]


viewHealthHistoryItem : Int -> Html msg
viewHealthHistoryItem delta =
    Html.p [] [ Html.text (String.fromInt delta) ]


viewEnergy : ( Energy, ( Cooldown, ( Int, Int ) ) ) -> Maybe (Html msg)
viewEnergy ( energy, ( cooldown, ( amount, cap ) ) ) =
    if amount > 0 then
        Just
            (Html.div [ Html.Attributes.class (Energy.toString energy) ]
                [ Html.p [] [ Html.text (Energy.toString energy ++ ": " ++ String.fromInt amount ++ "/" ++ String.fromInt cap) ]
                , viewCooldown cooldown
                ]
            )

    else
        Nothing


viewCardCost : ( Energy, Int ) -> Html msg
viewCardCost ( energy, amount ) =
    Html.p [ Html.Attributes.class (Energy.toString energy) ] [ Html.text (Energy.toString energy ++ ": " ++ String.fromInt amount) ]


viewCard : Character -> Card -> Html msg
viewCard character card =
    Html.div
        [ Html.Attributes.class "card"
        , Html.Attributes.classList [ ( "can-afford", Character.canAfford character card.cost ) ]
        ]
        [ Html.h3 [] [ Html.text card.name ]
        , Html.div [] (card.cost |> Dict.toList |> List.map viewCardCost)
        , Html.p [] [ Html.text (Card.actionToString card.action) ]
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
        , Html.div [] (Dict.toList character.energy |> List.filterMap viewEnergy)
        , Html.div [ Html.Attributes.class "hand" ] (List.map (viewCard character) character.hand)
        , Html.details []
            (Html.summary [] [ Html.text "Stats" ]
                :: (Character.deriveStats character
                        |> List.map viewStat
                   )
            )
        , Html.ul [ Html.Attributes.class "buffs" ] (List.map viewBuff character.buffs)
        ]


viewTurnState : TurnState -> Html Msg
viewTurnState turnState =
    Html.div [ Html.Attributes.class "turn-state" ]
        [ Html.p [] [ Html.text (turnStateString turnState) ]
        , Html.button [ Html.Events.onClick ClickedResetEnemy ] [ Html.text "reset enemy" ]
        ]


viewAttack : Attack -> Html Msg
viewAttack attack =
    Html.button [ Html.Events.onClick (ClickedSetPlayerAttack attack) ]
        [ Html.p [] [ Html.text attack.name ]
        , Html.p [] [ Html.text ("power: " ++ String.fromInt attack.power) ]

        -- , Html.p [] [ Html.text ("cost: " ++ Debug.toString attack.cost) ]
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "characters" ]
            (model.characters
                |> Dict.toList
                |> List.map (viewCharacter model.turnState)
            )
        , viewTurnState model.turnState
        , Html.div [ Html.Attributes.class "test-cards" ]
            (List.map viewAttack [ Attack "Basic attack" (Dict.fromList [ ( Energy.Cyan, 1 ) ]) 1 ])
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
