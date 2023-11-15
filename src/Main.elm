module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Character exposing (Character)
import Cooldown
import CustomDict as Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes



-- CONTENT


playerCharacter : Character
playerCharacter =
    Character.new 1000 3 2 100


enemyCharacter : Character
enemyCharacter =
    Character.new 1000 2 1 20



-- TURN STATE


type CharacterType
    = Player
    | Enemy


type TurnState
    = Recovering
    | Attacking CharacterType Cooldown.Cooldown



-- MODEL


type alias Model =
    { characters : Dict CharacterType Character
    , turnState : TurnState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( Player, playerCharacter )
            , ( Enemy, enemyCharacter )
            ]
        )
        Recovering
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickCharacterCooldowns dt
                -- |> playerCombat
                -- |> enemyCombat
                |> tickTurnState dt
                |> advanceTurnState


tickCharacterCooldowns : Float -> Model -> Model
tickCharacterCooldowns dt model =
    case model.turnState of
        Recovering ->
            { model | characters = model.characters |> Dict.map (\_ character -> Character.tickCooldown dt character) }

        _ ->
            model



-- playerCombat : Model -> Model
-- playerCombat model =
--     case model.turnState of
--         Recovering ->
--             if Character.isReady model.player then
--                 { model
--                     | turnState = PlayerAttacking (Cooldown.new 500)
--                 }
--             else
--                 model
--         _ ->
--             model


tickTurnState : Float -> Model -> Model
tickTurnState dt model =
    case model.turnState of
        Recovering ->
            model

        Attacking character cooldown ->
            { model | turnState = Attacking character (Cooldown.tick dt cooldown) }


advanceTurnState : Model -> Model
advanceTurnState model =
    case model.turnState of
        Recovering ->
            model

        Attacking character cooldown ->
            if Cooldown.isDone cooldown then
                { model
                    | turnState = Recovering
                    , characters = Dict.update character Character.resetCooldown model.characters
                }

            else
                model



-- VIEW


viewCharacter : Character -> Html msg
viewCharacter character =
    Html.div
        [ Html.Attributes.class "character"
        ]
        [ Html.p [] [ Html.text ("atk: " ++ String.fromInt character.attack) ]
        , Html.p [] [ Html.text ("spd: " ++ String.fromInt character.speed) ]
        , Html.p []
            [ Html.text
                ("hlt: "
                    ++ String.fromInt (Tuple.first character.health)
                    ++ "/"
                    ++ String.fromInt (Tuple.second character.health)
                )
            ]
        , Html.progress
            [ Html.Attributes.value (String.fromFloat (Tuple.first character.cooldown))
            , Html.Attributes.max (String.fromFloat (Tuple.second character.cooldown))
            ]
            []
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "characters" ]
            (Dict.toList model.characters |> List.map Tuple.second |> List.map viewCharacter)
        , Html.div []
            [ Html.p [] [ Html.text (Debug.toString model.turnState) ]
            ]
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
