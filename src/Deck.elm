module Deck exposing
    ( Action(..)
    , Card
    , Deck
    , actionToIcon
    , actionToString
    , addCard
    , canAfford
    , drawHand
    , new
    , newCard
    , playCardAtIndex
    , resetCards
    , tickEnergy
    )

import Minion exposing (Minion)


type alias Deck =
    { cards : List Card
    , hand : List Card
    , played : List Card
    , energy : Float
    , maxEnergy : Int
    , handSize : Int
    }


new : List Card -> Deck
new cards =
    Deck cards [] [] 0 10 3


{-| Put all cards back in deck
-}
resetCards : Deck -> Deck
resetCards deck =
    { deck
        | cards = deck.cards ++ deck.hand ++ deck.played
        , hand = []
        , played = []
    }


addCard : Card -> Deck -> Deck
addCard card deck =
    if List.length deck.hand < deck.handSize then
        { deck | hand = card :: deck.hand }

    else
        { deck | cards = card :: deck.cards }


drawHand : Deck -> Deck
drawHand deck =
    { deck
        | hand = List.take deck.handSize deck.cards
        , cards = List.drop deck.handSize deck.cards
    }


drawCard : Deck -> Deck
drawCard deck =
    case List.head deck.cards of
        Just card ->
            { deck
                | hand = deck.hand ++ [ card ]
                , cards = List.drop 1 deck.cards
            }

        Nothing ->
            if List.isEmpty deck.played |> not then
                { deck
                    | cards = deck.cards ++ List.reverse deck.played
                    , played = []
                }
                    |> drawCard

            else
                deck


{-| Attempt to play card at provided index from character hand.

Returns updated character and card action if card exists and character can afford it

-}
playCardAtIndex : Int -> Deck -> ( Deck, Maybe Action )
playCardAtIndex index deck =
    let
        getCard =
            deck.hand
                |> List.drop index
                |> List.head

        removeCard char =
            { char | hand = List.take index char.hand ++ List.drop (index + 1) char.hand }

        addToPlayed card char =
            { char | played = card :: char.played }
    in
    case getCard of
        Just card ->
            if canAfford deck card.cost then
                ( deck
                    |> removeEnergy card.cost
                    |> removeCard
                    |> addToPlayed card
                    |> drawCard
                , Just card.action
                )

            else
                ( deck, Nothing )

        Nothing ->
            ( deck, Nothing )



-- ENERGY


{-| Get a dict of current character energy
-}
getEnergy : Deck -> Int
getEnergy deck =
    floor deck.energy


canAfford : Deck -> Int -> Bool
canAfford deck cost =
    getEnergy deck >= cost



-- {-| Can character afford the first card in their hand. Return False if no cards are present
-- -}
-- canPlayFirst : Deck -> Bool
-- canPlayFirst deck =
--     deck.hand
--         |> List.head
--         |> Maybe.map (\card -> canAfford deck card.cost)
--         |> Maybe.withDefault False


tickEnergy : Float -> Deck -> Deck
tickEnergy dt deck =
    { deck | energy = deck.energy + (dt / 3500) |> min 10 }


removeEnergy : Int -> Deck -> Deck
removeEnergy cost deck =
    { deck | energy = deck.energy - toFloat cost |> max 0 }



-- Card


type Action
    = Damage Int
    | Summon Minion


actionToString : Action -> String
actionToString action =
    case action of
        Damage power ->
            "Damage " ++ String.fromInt power

        Summon minion ->
            "Summon " ++ String.fromChar minion.icon


actionToIcon : Action -> String
actionToIcon action =
    case action of
        Damage power ->
            "🗡️" ++ String.fromInt power

        Summon minion ->
            "s" ++ String.fromChar minion.icon


type alias Card =
    { icon : Char
    , name : String
    , action : Action
    , cost : Int
    }


newCard : Char -> String -> Action -> Int -> Card
newCard icon name action cost =
    Card icon name action cost
