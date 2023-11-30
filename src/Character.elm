module Character exposing
    ( Character
    , addCard
    , applyAction
    , canAfford
    , canPlayFirst
    , drawHand
    , generateDrops
    , isAlive
    , isReady
    , new
    , playCardAtIndex
    , removeEnergy
    , resetCards
    , resetCooldown
    , setDeck
    , setDropTable
    , tick
    )

import Card exposing (Action, Card)
import Cooldown exposing (Cooldown)
import Random exposing (Generator)


{-| Main Character type
-}
type alias Character =
    { idCounter : Int
    , icon : Char
    , health : ( Int, Int )
    , healthHistory : List ( Int, Int )
    , speed : Int
    , cooldown : Cooldown
    , ability : Action
    , energy : Float
    , deck : List Card
    , hand : List Card
    , played : List Card
    , dropTable : Maybe ( ( Float, Card ), List ( Float, Card ) )
    }


{-| Character constructor
-}
new : Char -> Int -> Action -> Int -> Character
new icon speed ability health =
    Character
        0
        icon
        ( health, health )
        []
        speed
        (Cooldown.new 5000)
        ability
        0
        []
        []
        []
        Nothing


{-| Put all cards back in deck
-}
resetCards : Character -> Character
resetCards character =
    { character
        | deck = character.deck ++ character.hand ++ character.played
        , hand = []
        , played = []
    }


addCard : Card -> Character -> Character
addCard card character =
    { character | deck = card :: character.deck }


setDeck : List Card -> Character -> Character
setDeck deck character =
    { character | deck = deck }


setDropTable : ( Float, Card ) -> List ( Float, Card ) -> Character -> Character
setDropTable first rest character =
    { character | dropTable = Just ( first, rest ) }


drawHand : Int -> Character -> Character
drawHand size character =
    { character | hand = character.deck |> List.take size, deck = character.deck |> List.drop size }


drawCard : Character -> Character
drawCard character =
    case List.head character.deck of
        Just card ->
            { character
                | hand = character.hand ++ [ card ]
                , deck = List.drop 1 character.deck
            }

        Nothing ->
            if List.isEmpty character.played |> not then
                { character
                    | deck = character.deck ++ List.reverse character.played
                    , played = []
                }
                    |> drawCard

            else
                character


{-| Attempt to play card at provided index from character hand.

Returns updated character and card action if card exists and character can afford it

-}
playCardAtIndex : Int -> Character -> ( Character, Maybe Action )
playCardAtIndex index character =
    let
        getCard =
            character.hand
                |> List.drop index
                |> List.head

        removeCard char =
            { char | hand = List.take index char.hand ++ List.drop (index + 1) char.hand }

        addToPlayed card char =
            { char | played = card :: char.played }
    in
    case getCard of
        Just card ->
            if canAfford character card.cost then
                ( character
                    |> removeEnergy card.cost
                    |> removeCard
                    |> addToPlayed card
                    |> drawCard
                , Just card.action
                )

            else
                ( character, Nothing )

        Nothing ->
            ( character, Nothing )


{-| tick character buff durations & energy regen.
-}
tick : Float -> Character -> Character
tick dt character =
    if isAlive character then
        { character
            | cooldown = Cooldown.tick (dt * toFloat character.speed) character.cooldown
            , energy = tickEnergy character dt character.energy
        }

    else
        character


resetCooldown : Character -> Character
resetCooldown character =
    { character | cooldown = Cooldown.reset character.cooldown }


{-| Apply hit to character
-}
hit : Int -> Character -> Character
hit power character =
    { character
        | health = character.health |> Tuple.mapFirst (\h -> max 0 (h - power))
        , healthHistory = ( character.idCounter, -power ) :: character.healthHistory |> List.take 10
        , idCounter = character.idCounter + 1
    }


applyAction : Action -> Character -> Character
applyAction action character =
    case action of
        Card.Damage power ->
            hit power character



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0


isReady : Character -> Bool
isReady character =
    Cooldown.isDone character.cooldown



-- ENERGY


{-| Get a dict of current character energy
-}
getEnergy : Character -> Int
getEnergy character =
    floor character.energy


canAfford : Character -> Int -> Bool
canAfford character cost =
    getEnergy character >= cost


{-| Can character afford the first card in their hand. Return False if no cards are present
-}
canPlayFirst : Character -> Bool
canPlayFirst character =
    character.hand
        |> List.head
        |> Maybe.map (\card -> canAfford character card.cost)
        |> Maybe.withDefault False


tickEnergy : Character -> Float -> Float -> Float
tickEnergy _ dt amount =
    amount + (dt / 3500) |> min 10


removeEnergy : Int -> Character -> Character
removeEnergy cost character =
    { character | energy = character.energy - toFloat cost |> max 0 }



-- DROPS


generateDrops : Character -> Generator (List Card)
generateDrops character =
    case character.dropTable of
        Just ( first, rest ) ->
            Random.list 3 (Random.weighted first rest)

        Nothing ->
            Random.constant []
