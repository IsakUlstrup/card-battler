module Character exposing
    ( Character
    , addCard
    , applyAction
    , canAfford
    , canPlayFirst
    , deriveStat
    , deriveStats
    , drawHand
    , isAlive
    , new
    , playCardAtIndex
    , removeEnergy
    , resetCards
    , tick
    )

import Buff exposing (Buff)
import Card exposing (Action, Card)
import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy(..))
import Stat exposing (Stat)


{-| Main Character type
-}
type alias Character =
    { icon : Char
    , health : ( Int, Int )
    , healthHistory : List Int
    , baseStats : Dict Stat Float
    , buffs : List Buff
    , energy : Dict Energy ( Cooldown, ( Int, Int ) )
    , deck : List Card
    , hand : List Card
    , played : List Card
    }


{-| Character constructor
-}
new : Char -> List Card -> List ( Stat, Float ) -> Int -> Character
new icon deck baseStats health =
    Character
        icon
        ( health, health )
        []
        (Dict.fromList baseStats)
        []
        (Dict.fromList
            [ ( Cyan, ( Cooldown.new 3500, ( 0, defaultEnergyCap ) ) )
            , ( Magenta, ( Cooldown.new 3500, ( 0, defaultEnergyCap ) ) )
            , ( Yellow, ( Cooldown.new 3500, ( 0, defaultEnergyCap ) ) )
            ]
        )
        deck
        []
        []


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
            | buffs =
                character.buffs
                    |> List.map (Buff.tick dt)
                    |> List.filter Buff.notDone
            , energy =
                character.energy
                    |> Dict.map (tickEnergy character dt)
                    |> Dict.map addEnergy
        }

    else
        character


{-| Apply hit to character
-}
hit : Int -> Character -> Character
hit power character =
    { character
        | health = character.health |> Tuple.mapFirst (\h -> max 0 (h - power))
        , healthHistory = -power :: character.healthHistory
    }


applyAction : Action -> Character -> Character
applyAction action character =
    case action of
        Card.Attack power ->
            hit power character

        Card.Buff buff ->
            addBuff buff character



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0



-- STATS


{-| Derive character stat of any type

Calculated using character base stat \* sum of any buffs or 1 if no buffs

-}
deriveStat : Stat -> Character -> Float
deriveStat stat character =
    let
        statBuffs : Float
        statBuffs =
            character.buffs
                |> List.filterMap
                    (\buff ->
                        if stat == Tuple.first buff.statModifier then
                            Just (Tuple.second buff.statModifier)

                        else
                            Nothing
                    )
                |> (\buffs ->
                        if not (List.isEmpty buffs) then
                            List.sum buffs

                        else
                            1
                   )
    in
    character.baseStats
        |> Dict.get stat
        |> Maybe.withDefault 0
        |> (\base -> base * statBuffs)


deriveStats : Character -> List ( Stat, Float )
deriveStats character =
    character.baseStats
        |> Dict.toList
        |> List.map Tuple.first
        |> List.map (\stat -> ( stat, deriveStat stat character ))



-- BUFF


{-| Add buff to character
-}
addBuff : Buff -> Character -> Character
addBuff buff character =
    { character | buffs = buff :: character.buffs }



-- ENERGY


{-| Get a dict of current character energy
-}
getEnergy : Character -> Dict Energy Int
getEnergy character =
    character.energy
        |> Dict.map (\_ v -> Tuple.second v |> Tuple.first)


canAfford : Character -> Dict Energy Int -> Bool
canAfford character cost =
    let
        characterEnergy : Dict Energy Int
        characterEnergy =
            getEnergy character

        hasEnergy : Energy -> Int -> Bool
        hasEnergy e c =
            Dict.get e characterEnergy |> Maybe.map (\me -> me >= c) |> Maybe.withDefault False
    in
    Dict.map hasEnergy cost |> Dict.all identity


{-| Can character afford the first card in their hand. Return False if no cards are present
-}
canPlayFirst : Character -> Bool
canPlayFirst character =
    character.hand
        |> List.head
        |> Maybe.map (\card -> canAfford character card.cost)
        |> Maybe.withDefault False


defaultEnergyCap : Int
defaultEnergyCap =
    10


tickEnergy : Character -> Float -> Energy -> ( Cooldown, ( Int, Int ) ) -> ( Cooldown, ( Int, Int ) )
tickEnergy character dt energy ( cooldown, ( amount, cap ) ) =
    if amount /= cap then
        case energy of
            Cyan ->
                ( Cooldown.tick (dt * deriveStat Stat.CyanRegenModifier character) cooldown, ( amount, cap ) )

            Magenta ->
                ( Cooldown.tick (dt * deriveStat Stat.MagentaRegenModifier character) cooldown, ( amount, cap ) )

            Yellow ->
                ( Cooldown.tick (dt * deriveStat Stat.YellowRegenModifier character) cooldown, ( amount, cap ) )

    else
        ( cooldown, ( amount, cap ) )


addEnergy : Energy -> ( Cooldown, ( Int, Int ) ) -> ( Cooldown, ( Int, Int ) )
addEnergy _ ( cooldown, ( amount, cap ) ) =
    if Cooldown.isDone cooldown then
        ( Cooldown.reset cooldown, ( min cap (amount + 1), cap ) )

    else
        ( cooldown, ( amount, cap ) )


removeEnergy : Dict Energy Int -> Character -> Character
removeEnergy cost character =
    let
        remove : Energy -> ( Cooldown, ( Int, Int ) ) -> ( Cooldown, ( Int, Int ) )
        remove energyType ( cooldown, energyState ) =
            case Dict.get energyType cost of
                Just energyCost ->
                    ( cooldown, Tuple.mapFirst (\e -> max 0 (e - energyCost)) energyState )

                Nothing ->
                    ( cooldown, energyState )
    in
    { character | energy = character.energy |> Dict.map remove }
