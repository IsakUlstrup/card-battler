module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Cooldown exposing (Cooldown)
import Html exposing (Html, main_)
import Html.Attributes



-- CHARACTER


type CharacterState
    = Idle Cooldown
    | Attacking Int Cooldown
    | Hit Int Cooldown


tickState : Float -> CharacterState -> CharacterState
tickState dt state =
    case state of
        Idle cd ->
            Idle (Cooldown.tick dt cd)

        Attacking attack cd ->
            Attacking attack (Cooldown.tick dt cd)

        Hit hit cd ->
            Hit hit (Cooldown.tick dt cd)


stateString : CharacterState -> String
stateString state =
    case state of
        Idle _ ->
            "idle"

        Attacking _ _ ->
            "attacking"

        Hit _ _ ->
            "hit"


type alias Character =
    { attack : Int
    , speed : Int
    , health : ( Int, Int )
    , state : CharacterState
    }


newCharacter : Float -> Int -> Int -> Int -> Character
newCharacter cooldown attack speed health =
    Character attack speed ( health, health ) (Idle (Cooldown.new cooldown))


playerCharacter : Character
playerCharacter =
    newCharacter 1000 3 2 100


enemyCharacter : Character
enemyCharacter =
    newCharacter 1000 2 1 20


tickCharacter : Float -> Character -> Character
tickCharacter dt character =
    if characterIsAlive character then
        { character | state = tickState (dt * toFloat character.speed) character.state }

    else
        character


advanceCharacterState : Character -> Character
advanceCharacterState character =
    let
        advanceState : CharacterState -> CharacterState
        advanceState state =
            case state of
                Idle cd ->
                    if Cooldown.isDone cd then
                        Attacking character.attack (Cooldown.new 500)

                    else
                        state

                Attacking _ cd ->
                    if Cooldown.isDone cd then
                        Idle (Cooldown.new 1000)

                    else
                        state

                Hit _ cd ->
                    if Cooldown.isDone cd then
                        Idle (Cooldown.new 1000)

                    else
                        state
    in
    { character | state = advanceState character.state }


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
    { model
        | player = tickCharacter dt model.player
        , enemy = tickCharacter dt model.enemy
    }


advanceCharacters : Model -> Model
advanceCharacters model =
    { model
        | player = advanceCharacterState model.player
        , enemy = advanceCharacterState model.enemy
    }


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
    let
        ( cooldown, maxCooldown ) =
            case character.state of
                Idle cd ->
                    cd

                _ ->
                    ( 0, 1000 )
    in
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
        , Html.p [] [ Html.text ("stt: " ++ Debug.toString character.state) ]
        , Html.progress
            [ Html.Attributes.value (String.fromFloat cooldown)
            , Html.Attributes.max (String.fromFloat maxCooldown)
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
