module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Character exposing (Character)
import Cooldown
import Html exposing (Html, main_)
import Html.Attributes



-- CONTENT


playerCharacter : Character
playerCharacter =
    Character.new 1000 3 2 100


enemyCharacter : Character
enemyCharacter =
    Character.new 1000 2 1 20



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
            if Character.isIdle target then
                character
                    |> Character.tickState dt
                    |> Character.tickCooldown dt

            else
                character
                    |> Character.tickCooldown dt
    in
    { model
        | player = helper model.enemy model.player
        , enemy = helper model.player model.enemy
    }


advanceCharacters : Model -> Model
advanceCharacters model =
    if Character.isIdle model.enemy && Cooldown.isDone model.player.cooldown then
        { model | player = Character.advanceState model.player }

    else
        { model | enemy = Character.advanceState model.enemy }


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
        , Html.Attributes.class (Character.stateString character.state)
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
        , Html.p [] [ Html.text ("stt: " ++ Character.stateIcon character.state) ]
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
