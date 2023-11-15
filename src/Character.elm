module Character exposing
    ( Character
    , hit
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
    { attack : Float
    , speed : Float
    , health : ( Int, Int )
    , cooldown : Cooldown
    }


{-| Character constructor
-}
new : Float -> Float -> Float -> Int -> Character
new cooldown attack speed health =
    Character attack speed ( health, health ) (Cooldown.new cooldown)


{-| tick character cooldown by given delta time.

Note: delta time will be multiplied by character speed

-}
tickCooldown : Float -> Character -> Character
tickCooldown dt character =
    if isAlive character then
        { character
            | cooldown = Cooldown.tick (dt * character.speed) character.cooldown
        }

    else
        character


resetCooldown : Character -> Character
resetCooldown character =
    { character | cooldown = Cooldown.reset character.cooldown }


hit : Int -> Character -> Character
hit power character =
    { character | health = character.health |> Tuple.mapFirst (\h -> max 0 (h - power)) }



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0


isReady : Character -> Bool
isReady character =
    Cooldown.isDone character.cooldown
