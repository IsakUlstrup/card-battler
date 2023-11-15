module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Character exposing (Character)
import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes



-- CONSTANTS


characterAnimationDuration : Float
characterAnimationDuration =
    300



-- CONTENT


playerCharacter : Character
playerCharacter =
    Character.new 2000 3 1 100


enemyCharacter : Character
enemyCharacter =
    Character.new 2500 2 1 20



-- TURN STATE


type CharacterType
    = Player
    | Enemy


notCharacterType : CharacterType -> CharacterType
notCharacterType type_ =
    case type_ of
        Player ->
            Enemy

        Enemy ->
            Player


characterTypeString : CharacterType -> String
characterTypeString type_ =
    case type_ of
        Player ->
            "player"

        Enemy ->
            "enemy"


type TurnState
    = Recovering
    | Attacking CharacterType Int Cooldown
    | Hit CharacterType Int Cooldown



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
                |> tickTurnState dt
                |> advanceTurnState


tickCharacterCooldowns : Float -> Model -> Model
tickCharacterCooldowns dt model =
    case model.turnState of
        Recovering ->
            { model | characters = model.characters |> Dict.map (\_ character -> Character.tickCooldown dt character) }

        _ ->
            model


tickTurnState : Float -> Model -> Model
tickTurnState dt model =
    case model.turnState of
        Recovering ->
            model

        Attacking character power cooldown ->
            { model | turnState = Attacking character power (Cooldown.tick dt cooldown) }

        Hit character hit cooldown ->
            { model | turnState = Hit character hit (Cooldown.tick dt cooldown) }


advanceTurnState : Model -> Model
advanceTurnState model =
    case model.turnState of
        Recovering ->
            let
                getReady =
                    model.characters
                        |> Dict.filter (\_ character -> Character.isReady character)
                        |> Dict.toList
                        |> List.head
            in
            case getReady of
                Just ( characterType, character ) ->
                    { model
                        | turnState = Attacking characterType character.attack (Cooldown.new characterAnimationDuration)
                    }

                _ ->
                    model

        Attacking character power cooldown ->
            if Cooldown.isDone cooldown then
                { model
                    | turnState = Hit (notCharacterType character) power (Cooldown.new characterAnimationDuration)
                    , characters = Dict.update character Character.resetCooldown model.characters
                }

            else
                model

        Hit _ _ cooldown ->
            if Cooldown.isDone cooldown then
                { model
                    | turnState = Recovering
                }

            else
                model



-- VIEW


viewCharacter : TurnState -> ( CharacterType, Character ) -> Html msg
viewCharacter turnState ( type_, character ) =
    let
        stateString =
            case turnState of
                Recovering ->
                    "idle"

                Attacking characterType _ _ ->
                    if characterType == type_ then
                        "attacking"

                    else
                        "idle"

                Hit characterType _ _ ->
                    if characterType == type_ then
                        "hit"

                    else
                        "idle"

        combatEffect =
            case turnState of
                Recovering ->
                    Nothing

                Attacking _ _ _ ->
                    Nothing

                Hit characterType hit _ ->
                    if characterType == type_ then
                        Just ("-" ++ String.fromInt hit)

                    else
                        Nothing
    in
    Html.div
        [ Html.Attributes.class "character"
        , Html.Attributes.class (characterTypeString type_)
        , Html.Attributes.class stateString
        ]
        ([ Html.p [] [ Html.text ("atk: " ++ String.fromInt character.attack) ]
         , Html.p [] [ Html.text ("spd: " ++ String.fromInt character.speed) ]
         , Html.p []
            [ Html.text
                ("hlt: "
                    ++ String.fromInt (Tuple.first character.health)
                    ++ "/"
                    ++ String.fromInt (Tuple.second character.health)
                )
            ]
         , Html.meter
            [ Html.Attributes.value (String.fromInt (Tuple.first character.health))
            , Html.Attributes.max (String.fromInt (Tuple.second character.health))
            ]
            []
         , Html.progress
            [ Html.Attributes.value (String.fromFloat (Tuple.first character.cooldown))
            , Html.Attributes.max (String.fromFloat (Tuple.second character.cooldown))
            ]
            []
         ]
            ++ ([ combatEffect ]
                    |> List.filterMap identity
                    |> List.map (\t -> Html.div [ Html.Attributes.class "combat-status" ] [ Html.p [] [ Html.text t ] ])
               )
        )


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.div [ Html.Attributes.class "characters" ]
            (model.characters
                |> Dict.toList
                |> List.map (viewCharacter model.turnState)
            )
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
