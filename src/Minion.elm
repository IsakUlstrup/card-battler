module Minion exposing (Minion, damage, isAlive, isReady, new, resetCooldown, tick)

import Cooldown exposing (Cooldown)


type alias Minion =
    { icon : Char
    , health : Int
    , speed : Int
    , ability : ( Cooldown, Int )
    }


defaultAbilityCooldown : Float
defaultAbilityCooldown =
    5000


{-| Minion constructor.

Has no drops by default. Set drops with setDroptable

-}
new : Char -> Int -> Int -> Int -> Minion
new icon health speed attack =
    Minion
        icon
        health
        speed
        ( Cooldown.new defaultAbilityCooldown, attack )



-- Nothing


{-| Tick ability cooldown if minion is alive

For now just multiply dt with minion speed. This probably needs improvement.

-}
tick : Float -> Minion -> Minion
tick dt minion =
    if isAlive minion then
        { minion | ability = minion.ability |> Tuple.mapFirst (Cooldown.tick (dt * toFloat minion.speed)) }

    else
        minion


resetCooldown : Minion -> Minion
resetCooldown minion =
    { minion | ability = Tuple.mapFirst Cooldown.reset minion.ability }


{-| Apply damage to minion
-}
damage : Int -> Minion -> Minion
damage amount minion =
    { minion | health = max 0 (minion.health - amount) }



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
