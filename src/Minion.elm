module Minion exposing (Minion, applyAction, generateDrops, new, setDroptable, tick)

import Card exposing (Action, Card)
import Character exposing (isAlive)
import Cooldown exposing (Cooldown)
import Random exposing (Generator)


type alias Minion =
    { icon : Char
    , health : Int
    , speed : Int
    , ability : ( Cooldown, Action )
    , dropTable : Maybe ( ( Float, Card ), List ( Float, Card ) )
    }


{-| Minion constructor.

Has no drops by default. Set drops with setDroptable

-}
new : Char -> Int -> Int -> ( Float, Action ) -> Minion
new icon health speed ( abilityCooldown, abilityAction ) =
    Minion
        icon
        health
        speed
        ( Cooldown.new abilityCooldown, abilityAction )
        Nothing


{-| Tick ability cooldown if minion is alive

For now just multiply dt with minion speed. This probably needs improvement.

-}
tick : Float -> Minion -> Minion
tick dt minion =
    if isAlive minion then
        { minion | ability = minion.ability |> Tuple.mapFirst (Cooldown.tick (dt * toFloat minion.speed)) }

    else
        minion


{-| Apply action to minion
-}
applyAction : Action -> Minion -> Minion
applyAction action minion =
    case action of
        Card.Damage power ->
            damage power minion


{-| Apply damage to minion
-}
damage : Int -> Minion -> Minion
damage amount minion =
    { minion | health = max 0 (minion.health - amount) }


{-| Set drop table.

Each entry is a tuple with a weight and a card. Higher weight means a card is more likely to be selected

-}
setDroptable : ( ( Float, Card ), List ( Float, Card ) ) -> Minion -> Minion
setDroptable dropTable minion =
    { minion | dropTable = Just dropTable }



-- Predicates


{-| Is minion alive?
-}
isAlive : Minion -> Bool
isAlive minion =
    minion.health > 0


{-| Is minion ability cooldown done?
-}
isReady : Minion -> Bool
isReady minion =
    Cooldown.isDone (Tuple.first minion.ability)



-- Drops


{-| Generate a list of drops based on drop table
-}
generateDrops : Minion -> Generator (List Card)
generateDrops minion =
    case minion.dropTable of
        Just ( first, rest ) ->
            Random.list 3 (Random.weighted first rest)

        Nothing ->
            Random.constant []
