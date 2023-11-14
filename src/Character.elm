module Character exposing
    ( Character
    , isAlive
    , isReady
    , new
    , resetCooldown
    , tickCooldown
    )

import Cooldown exposing (Cooldown)


{-| Main Character type
-}
type alias Character =
    { attack : Int
    , speed : Int
    , health : ( Int, Int )
    , cooldown : Cooldown
    }


{-| Character constructor
-}
new : Float -> Int -> Int -> Int -> Character
new cooldown attack speed health =
    Character attack speed ( health, health ) (Cooldown.new cooldown)


{-| tick character cooldown by given delta time.

Note: delta time will be multiplied by character speed

-}
tickCooldown : Float -> Character -> Character
tickCooldown dt character =
    if isAlive character then
        { character
            | cooldown = Cooldown.tick (dt * toFloat character.speed) character.cooldown
        }

    else
        character


resetCooldown : Character -> Character
resetCooldown character =
    { character | cooldown = Cooldown.reset character.cooldown }



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0


isReady : Character -> Bool
isReady character =
    Cooldown.isDone character.cooldown
