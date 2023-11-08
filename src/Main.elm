module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import CustomDict as Dict exposing (Dict)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Html.Keyed



-- DECK


type alias Deck =
    { draw : List ( Int, Bool, Card )
    , hand : List ( Int, Bool, Card )
    , handSize : Int
    }


deckFromList : Int -> List Card -> Deck
deckFromList handSize cards =
    Deck (List.indexedMap (\i c -> ( i, False, c )) cards) [] handSize


deckNext : Deck -> Deck
deckNext deck =
    case deck.draw of
        [] ->
            if List.isEmpty deck.draw then
                { deck
                    | draw = List.map (\( id, _, card ) -> ( id, False, card )) deck.hand
                    , hand = []
                }

            else
                deck

        card :: rest ->
            { deck
                | hand = card :: deck.hand |> List.take (deck.handSize + 1)
                , draw =
                    rest
                        ++ (card
                                :: deck.hand
                                |> List.drop (deck.handSize + 1)
                                |> List.map (\( id, _, c ) -> ( id, False, c ))
                           )
            }


mapPlayed : (Int -> Bool -> Bool) -> Deck -> Deck
mapPlayed f deck =
    { deck
        | draw = List.map (\( id, played, card ) -> ( id, f id played, card )) deck.draw
        , hand = List.map (\( id, played, card ) -> ( id, f id played, card )) deck.hand
    }



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


testCard4 : Card
testCard4 =
    Card "Expensive card2" (Dict.fromList [ ( Yellow, 4 ), ( Orange, 2 ) ]) (IncreaseEnergyMax Yellow 5)



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


discardCard : Int -> Card -> Model -> Model
discardCard targetIndex _ model =
    let
        helper : Int -> Bool -> Bool
        helper id played =
            if id == targetIndex then
                not played

            else
                played
    in
    { model
        | deck = mapPlayed helper model.deck
    }


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


tickDrawCooldown : Float -> Model -> Model
tickDrawCooldown dt model =
    { model | drawCooldown = tickCooldown dt model.drawCooldown }


drawIfReady : Model -> Model
drawIfReady model =
    if isDone model.drawCooldown then
        drawCard { model | drawCooldown = reset model.drawCooldown }

    else
        model


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


drawCard : Model -> Model
drawCard model =
    { model | deck = deckNext model.deck }


type alias PlayerEnergy =
    { current : Int
    , max : Int
    , cooldown : Cooldown
    , cooldownrate : Float
    }


type alias Model =
    { deck : Deck
    , drawCooldown : Cooldown
    , playerEnergy : Dict Energy PlayerEnergy
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (deckFromList
            5
            [ testCard
            , testCard2
            , testCard3
            , testCard4
            , testCard2
            , testCard3
            , testCard4
            , testCard2
            , testCard3
            , testCard4
            , testCard2
            , testCard3
            , testCard4
            , testCard2
            , testCard3
            , testCard4
            ]
        )
        (newCooldown 2000)
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
                |> tickDrawCooldown dt
                |> recoverEnergy
                |> drawIfReady

        -- |> discardIfOverflow
        ClickedCard index card ->
            if canAffordCost card.cost model.playerEnergy then
                model
                    |> applyCard card
                    |> discardCard index card

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
                            , ( energyString e, True )
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


viewCardEffect : CardEffect -> Html msg
viewCardEffect cardEffect =
    case cardEffect of
        IncreaseEnergyMax energyType amount ->
            Html.p [ Html.Attributes.class (energyString energyType) ] [ Html.text ("+" ++ String.fromInt amount ++ " max " ++ energyString energyType) ]


viewCard : Dict Energy PlayerEnergy -> ( Int, Bool, Card ) -> Html Msg
viewCard playerEnergy ( id, played, card ) =
    Html.div
        [ Html.Attributes.class "card"
        , Html.Attributes.classList
            [ ( "card", True )
            , ( "can-afford", canAffordCost card.cost playerEnergy )
            , ( "played", played )
            ]
        , Html.Events.onClick (ClickedCard id card)
        ]
        [ Html.h1 [] [ Html.text card.name ]
        , Html.p [] [ Html.text ("id" ++ String.fromInt id) ]
        , viewCardCost playerEnergy card.cost
        , viewCardEffect card.effect
        ]


viewKeyedCard : Dict Energy PlayerEnergy -> ( Int, Bool, Card ) -> ( String, Html Msg )
viewKeyedCard playerEnergy ( id, played, card ) =
    ( String.fromInt id
    , viewCard playerEnergy ( id, played, card )
    )


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


viewDeckControls : Model -> Html Msg
viewDeckControls model =
    Html.div [ Html.Attributes.class "hand-controls" ]
        [ Html.div [] [ Html.text "draw cooldown:", viewCooldownProgress model.drawCooldown ]
        , Html.p [] [ Html.text ("deck: " ++ String.fromInt (List.length model.deck.draw)) ]
        , Html.p [] [ Html.text ("hand size: " ++ String.fromInt model.deck.handSize) ]
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ viewPlayerEnergy model.playerEnergy
        , viewDeckControls model
        , Html.Keyed.node "div"
            [ Html.Attributes.class "hand" ]
            (List.map (viewKeyedCard model.playerEnergy) model.deck.hand)
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
