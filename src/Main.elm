module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Cooldown exposing (Cooldown)
import Html exposing (Html, main_)
import Html.Attributes



-- CHARACTER


type CharacterState
    = Idle
    | Attacking Int Cooldown
    | Hit Int Cooldown


tickState : Float -> CharacterState -> CharacterState
tickState dt state =
    case state of
        Idle ->
            Idle

        Attacking attack cd ->
            Attacking attack (Cooldown.tick dt cd)

        Hit hit cd ->
            Hit hit (Cooldown.tick dt cd)


stateString : CharacterState -> String
stateString state =
    case state of
        Idle ->
            "idle"

        Attacking _ _ ->
            "attacking"

        Hit _ _ ->
            "hit"


stateIcon : CharacterState -> String
stateIcon state =
    case state of
        Idle ->
            "😴"

        Attacking _ _ ->
            "🗡️"

        Hit _ _ ->
            "🤕"


type alias Character =
    { attack : Int
    , speed : Int
    , health : ( Int, Int )
    , state : CharacterState
    , cooldown : Cooldown

    -- TODO: move idle cooldown to cooldown tp prevent stun locking
    }


newCharacter : Float -> Int -> Int -> Int -> Character
newCharacter cooldown attack speed health =
    Character attack speed ( health, health ) Idle (Cooldown.new cooldown)


playerCharacter : Character
playerCharacter =
    newCharacter 1000 3 2 100


enemyCharacter : Character
enemyCharacter =
    newCharacter 1000 2 1 20


tickCharacterState : Float -> Character -> Character
tickCharacterState dt character =
    if characterIsAlive character then
        { character
            | state = tickState dt character.state
        }

    else
        character


tickCharacterCooldown : Float -> Character -> Character
tickCharacterCooldown dt character =
    if characterIsAlive character then
        { character
            | cooldown = Cooldown.tick (dt * toFloat character.speed) character.cooldown
        }

    else
        character


isIdle : Character -> Bool
isIdle character =
    case character.state of
        Idle ->
            True

        _ ->
            False


advanceCharacterState : Character -> Character
advanceCharacterState character =
    case character.state of
        Idle ->
            if Cooldown.isDone character.cooldown then
                { character | state = Attacking character.attack (Cooldown.new 1000) }

            else
                character

        Attacking _ cd ->
            if Cooldown.isDone cd then
                { character
                    | state = Idle
                    , cooldown = Cooldown.reset character.cooldown
                }

            else
                character

        Hit _ cd ->
            if Cooldown.isDone cd then
                { character
                    | state = Idle
                    , cooldown = Cooldown.reset character.cooldown
                }

            else
                character


characterHit : Int -> Character -> Character
characterHit hit character =
    { character | health = Tuple.mapFirst (\h -> max 0 (h - hit)) character.health }


characterIsAlive : Character -> Bool
characterIsAlive character =
    Tuple.first character.health > 0



-- MODEL


type alias Model =
    { player : Character
    , enemy : Character
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        playerCharacter
        enemyCharacter
    , Cmd.none
    )



-- UPDATE


tickCharacterCooldowns : Float -> Model -> Model
tickCharacterCooldowns dt model =
    let
        helper target character =
            case target.state of
                Idle ->
                    character
                        |> tickCharacterState dt
                        |> tickCharacterCooldown dt

                _ ->
                    character
                        |> tickCharacterState dt
    in
    { model
        | player = helper model.enemy model.player
        , enemy = helper model.player model.enemy
    }


advanceCharacters : Model -> Model
advanceCharacters model =
    if isIdle model.enemy && Cooldown.isDone model.player.cooldown then
        { model | player = advanceCharacterState model.player }

    else
        { model | enemy = advanceCharacterState model.enemy }


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickCharacterCooldowns dt
                |> advanceCharacters



-- VIEW


viewCharacter : Character -> Html msg
viewCharacter character =
    Html.div
        [ Html.Attributes.class "character"
        , Html.Attributes.class (stateString character.state)
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
        , Html.p [] [ Html.text ("stt: " ++ stateIcon character.state) ]
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
            [ viewCharacter model.player
            , viewCharacter model.enemy
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
