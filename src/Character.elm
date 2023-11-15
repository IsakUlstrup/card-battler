module Character exposing
    ( Character
    , Stat(..)
    , deriveAttack
    , deriveSpeed
    , hit
    , isAlive
    , isReady
    , new
    , resetCooldown
    , tickCooldown
    )

import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)


type Stat
    = Attack
    | Speed


{-| Main Character type
-}
type alias Character =
    { health : ( Int, Int )
    , cooldown : Cooldown
    , baseStats : Dict Stat Float
    }


{-| Character constructor
-}
new : List ( Stat, Float ) -> Float -> Int -> Character
new baseStats cooldown health =
    Character
        ( health, health )
        (Cooldown.new cooldown)
        (Dict.fromList baseStats)


{-| tick character cooldown by given delta time.

Note: delta time will be multiplied by character speed

-}
tickCooldown : Float -> Character -> Character
tickCooldown dt character =
    if isAlive character then
        { character
            | cooldown = Cooldown.tick (dt * deriveSpeed character) character.cooldown
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



-- STATS


deriveSpeed : Character -> Float
deriveSpeed character =
    character.baseStats
        |> Dict.get Speed
        |> Maybe.withDefault 1


deriveAttack : Character -> Float
deriveAttack character =
    character.baseStats
        |> Dict.get Attack
        |> Maybe.withDefault 1
