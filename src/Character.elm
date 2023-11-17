module Character exposing
    ( Buff
    , Character
    , Stat(..)
    , addBuff
    , deriveAttack
    , deriveSpeed
    , deriveStats
    , hit
    , isAlive
    , isReady
    , new
    , newBuff
    , resetCooldown
    , statString
    , tickBuffs
    , tickCooldown
    )

import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)


{-| Main Character type
-}
type alias Character =
    { health : ( Int, Int )
    , healthHistory : List Int
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
        []
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


{-| Tick buff durations and remove any that are done
-}
tickBuffs : Float -> Character -> Character
tickBuffs dt character =
    { character
        | buffs =
            character.buffs
                |> List.map (tickBuff dt)
                |> List.filter buffNotDone
    }


{-| Reset character cooldown
-}
resetCooldown : Character -> Character
resetCooldown character =
    { character | cooldown = Cooldown.reset character.cooldown }


{-| Apply hit to character
-}
hit : Int -> Character -> Character
hit power character =
    { character
        | health = character.health |> Tuple.mapFirst (\h -> max 0 (h - power))
        , healthHistory = power :: character.healthHistory
    }



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0


{-| Is character cooldown done
-}
isReady : Character -> Bool
isReady character =
    Cooldown.isDone character.cooldown



-- STATS


{-| Holds all stat types
-}
type Stat
    = Attack
    | Speed


{-| stat to string
-}
statString : Stat -> String
statString stat =
    case stat of
        Attack ->
            "🗡️"

        Speed ->
            "⚡"


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
    [ Attack
    , Speed
    ]
        |> List.map (\stat -> ( stat, deriveStat stat character ))


{-| Derive character speed stat
-}
deriveSpeed : Character -> Float
deriveSpeed character =
    deriveStat Speed character


{-| Derive character attack stat
-}
deriveAttack : Character -> Float
deriveAttack character =
    deriveStat Attack character



-- BUFF


{-| A buff is a temporary modifier to one stat
-}
type alias Buff =
    { duration : Cooldown
    , statModifier : ( Stat, Float )
    }


{-| Buff constructor
-}
newBuff : Float -> ( Stat, Float ) -> Buff
newBuff duration statModifier =
    Buff (Cooldown.new duration) statModifier


{-| Add buff to character
-}
addBuff : Buff -> Character -> Character
addBuff buff character =
    { character | buffs = buff :: character.buffs }


{-| Tick buff duration
-}
tickBuff : Float -> Buff -> Buff
tickBuff dt buff =
    { buff | duration = Cooldown.tick dt buff.duration }


{-| True if buff is not done
-}
buffNotDone : Buff -> Bool
buffNotDone buff =
    Cooldown.isDone buff.duration |> not
