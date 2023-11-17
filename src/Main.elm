module Main exposing (Model, Msg, TurnState, main)

import Browser
import Browser.Events
import Character exposing (Buff, Character, Stat)
import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    300



-- CONTENT


playerCharacter : Character
playerCharacter =
    Character.new [ ( Character.Speed, 2 ) ] 5000 100


enemyCharacter : Character
enemyCharacter =
    Character.new [] 7500 20



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
    | Hit Bool Int Cooldown
    | Done Bool


turnStateString : TurnState -> String
turnStateString turnState =
    case turnState of
        Recovering ->
            "recovering"

        Attacking _ _ _ ->
            "attacking"

        Hit _ _ _ ->
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
    | ClickedAddBuff Bool Buff


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

        ClickedAddBuff character buff ->
            { model | characters = Dict.update character (Character.addBuff buff) model.characters }


tickCharacters : Float -> Model -> Model
tickCharacters dt model =
    case ( model.turnState, Dict.all Character.isAlive model.characters ) of
        ( Recovering, True ) ->
            { model
                | characters =
                    model.characters
                        |> Dict.map (\_ character -> Character.tickCooldown dt character)
                        |> Dict.map (\_ character -> Character.tickBuffs dt character)
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

        Hit isPlayer hit cooldown ->
            { model | turnState = Hit isPlayer hit (Cooldown.tick dt cooldown) }

        Done _ ->
            model


getHead : (Character -> Bool) -> Model -> Maybe ( Bool, Character )
getHead f model =
    model.characters
        |> Dict.filter (\_ character -> f character)
        |> Dict.toList
        |> List.head


advanceTurnState : Model -> Model
advanceTurnState model =
    case model.turnState of
        Recovering ->
            case ( getHead (Character.isAlive >> not) model, getHead Character.isReady model ) of
                ( Nothing, Just ( isPlayer, character ) ) ->
                    { model
                        | turnState = Attacking isPlayer (round (Character.deriveAttack character)) (Cooldown.new characterAnimationDuration)
                    }

                ( Just ( charType, _ ), _ ) ->
                    { model | turnState = Done (not charType) }

                _ ->
                    model

        Attacking isPlayer power cooldown ->
            if Cooldown.isDone cooldown then
                { model
                    | turnState = Hit (not isPlayer) power (Cooldown.new characterAnimationDuration)
                    , characters = Dict.update isPlayer Character.resetCooldown model.characters
                }

            else
                model

        Hit character hit cooldown ->
            if Cooldown.isDone cooldown then
                { model
                    | turnState = Recovering
                    , characters = Dict.update character (Character.hit hit) model.characters
                }

            else
                model

        Done _ ->
            model



-- VIEW


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
    Html.p [] [ Html.text ("-" ++ String.fromInt delta) ]


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
                Hit characterType _ _ ->
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
        , Html.p []
            [ Html.text
                ("hlt: "
                    ++ String.fromInt (Tuple.first character.health)
                    ++ "/"
                    ++ String.fromInt (Tuple.second character.health)
                )
            ]
        , Html.div [ Html.Attributes.class "health-history" ] (List.map viewHealthHistoryItem character.healthHistory)
        , Html.meter
            [ Html.Attributes.value (String.fromInt (Tuple.first character.health))
            , Html.Attributes.max (String.fromInt (Tuple.second character.health))
            ]
            []
        , viewCooldown character.cooldown
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


viewBuffPresets : Html Msg
viewBuffPresets =
    let
        viewBuffPreset : String -> Bool -> Buff -> Html Msg
        viewBuffPreset label isPlayer buff =
            Html.button [ Html.Events.onClick (ClickedAddBuff isPlayer buff) ] [ Html.text label ]
    in
    Html.div [ Html.Attributes.class "buff-presets" ]
        [ viewBuffPreset "Buff player speed" True (Character.newBuff 1000 ( Character.Speed, 2 ))
        , viewBuffPreset "Debuff enemy speed" False (Character.newBuff 1000 ( Character.Speed, 0.2 ))
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
        , viewBuffPresets
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
