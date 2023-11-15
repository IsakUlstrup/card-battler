module Character exposing
    ( Buff
    , Character
    , Stat(..)
    , addBuff
    , deriveAttack
    , deriveSpeed
    , hit
    , isAlive
    , isReady
    , new
    , newBuff
    , resetCooldown
    , statString
    , tickCooldown
    )

import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)


{-| Main Character type
-}
type alias Character =
    { health : ( Int, Int )
    , cooldown : Cooldown
    , baseStats : Dict Stat Float
    , buffs : List Buff
    }


{-| Character constructor
-}
new : List ( Stat, Float ) -> Float -> Int -> Character
new baseStats cooldown health =
    Character
        ( health, health )
        (Cooldown.new cooldown)
        (Dict.fromList baseStats)
        []


{-| tick character cooldown by given delta time.

Note: delta time will be multiplied by character speed

-}
tickCooldown : Float -> Character -> Character
tickCooldown dt character =
    if isAlive character then
        { character
            | cooldown = Cooldown.tick (dt * deriveSpeed character) character.cooldown
            , buffs =
                character.buffs
                    |> List.map (tickBuff dt)
                    |> List.filter buffNotDone
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


type Stat
    = Attack
    | Speed


statString : Stat -> String
statString stat =
    case stat of
        Attack ->
            "attack"

        Speed ->
            "speed"


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
        |> Maybe.withDefault 1
        |> (\base -> base * statBuffs)


deriveSpeed : Character -> Float
deriveSpeed character =
    deriveStat Speed character


deriveAttack : Character -> Float
deriveAttack character =
    deriveStat Attack character



-- BUFF


type alias Buff =
    { duration : Cooldown
    , statModifier : ( Stat, Float )
    }


newBuff : Float -> ( Stat, Float ) -> Buff
newBuff duration statModifier =
    Buff (Cooldown.new duration) statModifier


addBuff : Buff -> Character -> Character
addBuff buff character =
    { character | buffs = buff :: character.buffs }


tickBuff : Float -> Buff -> Buff
tickBuff dt buff =
    { buff | duration = Cooldown.tick dt buff.duration }


buffNotDone : Buff -> Bool
buffNotDone buff =
    Cooldown.isDone buff.duration |> not
