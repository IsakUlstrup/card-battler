module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import CustomDict as Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events



-- COOLDOWN


type alias Cooldown =
    ( Float, Float )


newCooldown : Float -> Cooldown
newCooldown duration =
    ( 0, duration )


tickCooldown : Float -> Cooldown -> Cooldown
tickCooldown dt ( cd, maxCd ) =
    ( min maxCd (cd + dt), maxCd )


isDone : Cooldown -> Bool
isDone ( cd, maxCd ) =
    cd >= maxCd


reset : Cooldown -> Cooldown
reset ( _, maxCd ) =
    ( 0, maxCd )



-- ENERGY


type Energy
    = Yellow
    | Orange


energyString : Energy -> String
energyString energyType_ =
    case energyType_ of
        Yellow ->
            "yellow"

        Orange ->
            "orange"


canAffordCost : Dict Energy Int -> Dict Energy PlayerEnergy -> Bool
canAffordCost cost energy =
    let
        canAfford : Energy -> Int -> Bool
        canAfford e i =
            case Dict.get e energy of
                Just playerEnergy ->
                    playerEnergy.current >= i

                Nothing ->
                    False
    in
    cost |> Dict.map canAfford |> Dict.all ((==) True)


subtractCost : Dict Energy Int -> Dict Energy PlayerEnergy -> Dict Energy PlayerEnergy
subtractCost _ energy =
    energy


increaseMaxEnergy : Energy -> Int -> Dict Energy PlayerEnergy -> Dict Energy PlayerEnergy
increaseMaxEnergy energyType_ amount energy =
    case Dict.get energyType_ energy of
        Just e ->
            Dict.insert energyType_ { e | max = e.max + amount } energy

        Nothing ->
            energy



-- CARD


type CardEffect
    = IncreaseEnergyMax Energy Int


type alias Card =
    { name : String
    , cost : Dict Energy Int
    , effect : CardEffect
    }


testCard : Card
testCard =
    Card "Yellow Energy" (Dict.fromList []) (IncreaseEnergyMax Yellow 1)


testCard2 : Card
testCard2 =
    Card "Orange Energy" (Dict.fromList []) (IncreaseEnergyMax Orange 1)


testCard3 : Card
testCard3 =
    Card "Expensive card" (Dict.fromList [ ( Yellow, 1 ), ( Orange, 2 ) ]) (IncreaseEnergyMax Yellow 5)



-- MODEL


applyCard : Card -> Model -> Model
applyCard card model =
    case card.effect of
        IncreaseEnergyMax energy amount ->
            { model
                | playerEnergy =
                    model.playerEnergy
                        |> subtractCost card.cost
                        |> increaseMaxEnergy energy amount
            }


removeCard : Int -> Model -> Model
removeCard targetIndex model =
    let
        removeIndex target index card =
            if target == index then
                Nothing

            else
                Just card
    in
    { model | cards = List.indexedMap (removeIndex targetIndex) model.cards |> List.filterMap identity }


tickEnergyCooldowns : Float -> Model -> Model
tickEnergyCooldowns dt model =
    { model
        | playerEnergy =
            model.playerEnergy
                |> Dict.map
                    (\_ pe ->
                        if pe.current < pe.max then
                            { pe | cooldown = tickCooldown (dt * pe.cooldownrate) pe.cooldown }

                        else
                            pe
                    )
    }


recoverEnergy : Model -> Model
recoverEnergy model =
    { model
        | playerEnergy =
            model.playerEnergy
                |> Dict.map
                    (\_ pe ->
                        if isDone pe.cooldown then
                            { pe
                                | current = pe.current + 1
                                , cooldown = reset pe.cooldown
                            }

                        else
                            pe
                    )
    }


type alias PlayerEnergy =
    { current : Int
    , max : Int
    , cooldown : Cooldown
    , cooldownrate : Float
    }


type alias Model =
    { cards : List Card
    , playerEnergy : Dict Energy PlayerEnergy
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ testCard
        , testCard
        , testCard2
        , testCard2
        , testCard3
        ]
        (Dict.fromList
            [ ( Yellow, PlayerEnergy 0 0 (newCooldown 1000) 1 )
            , ( Orange, PlayerEnergy 0 0 (newCooldown 1000) 0.1 )
            ]
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ClickedCard Int Card


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> tickEnergyCooldowns dt
                |> recoverEnergy

        ClickedCard index card ->
            if canAffordCost card.cost model.playerEnergy then
                model
                    |> applyCard card
                    |> removeCard index

            else
                model



-- VIEW


viewCardCost : Dict Energy PlayerEnergy -> Dict Energy Int -> Html msg
viewCardCost playerEnergy cost =
    let
        playerHasEnergy e c =
            case Dict.get e playerEnergy of
                Just elementEnergy ->
                    elementEnergy.current >= c

                Nothing ->
                    False

        viewCost : Energy -> Int -> Maybe (Html msg)
        viewCost e c =
            let
                viewHelper s =
                    Html.li
                        [ Html.Attributes.classList
                            [ ( "has-energy", playerHasEnergy e c )
                            , ( "insufficient-energy", playerHasEnergy e c |> not )
                            ]
                        ]
                        [ Html.text s ]
            in
            case c of
                0 ->
                    Nothing

                1 ->
                    Just
                        (viewHelper (energyString e))

                _ ->
                    Just
                        (viewHelper (energyString e ++ " " ++ String.fromInt c))
    in
    Html.ul [ Html.Attributes.class "energy-cost" ]
        ((cost
            |> Dict.map viewCost
            |> Dict.toList
            |> List.map Tuple.second
         )
            |> List.filterMap identity
        )


viewCard : Dict Energy PlayerEnergy -> Int -> Card -> Html Msg
viewCard playerEnergy index card =
    Html.div
        [ Html.Attributes.class "card"
        , Html.Attributes.classList
            [ ( "card", True )
            , ( "can-afford", canAffordCost card.cost playerEnergy )
            ]
        , Html.Events.onClick (ClickedCard index card)
        ]
        [ Html.h1 [] [ Html.text card.name ]
        , viewCardCost playerEnergy card.cost
        , Html.p [] [ Html.text (Debug.toString card.effect) ]
        ]


viewEnergyMeter : Energy -> PlayerEnergy -> Html msg
viewEnergyMeter energyType_ energy =
    Html.li []
        [ Html.text (energyString energyType_ ++ " " ++ String.fromInt energy.current ++ "/" ++ String.fromInt energy.max)
        , viewCooldownProgress energy.cooldown
        ]


viewPlayerEnergy : Dict Energy PlayerEnergy -> Html Msg
viewPlayerEnergy energy =
    Html.div []
        [ Html.p [] [ Html.text "Player Energy" ]
        , Html.ul [ Html.Attributes.class "energy-meters" ]
            (energy
                |> Dict.map viewEnergyMeter
                |> Dict.toList
                |> List.map Tuple.second
            )
        ]


viewCooldownProgress : Cooldown -> Html msg
viewCooldownProgress ( cd, maxCd ) =
    Html.progress
        [ Html.Attributes.max (String.fromFloat maxCd)
        , Html.Attributes.value (String.fromFloat cd)
        ]
        []


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ viewPlayerEnergy model.playerEnergy
        , Html.div [ Html.Attributes.class "cards" ] (List.indexedMap (viewCard model.playerEnergy) model.cards)
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
