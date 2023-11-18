module Character exposing
    ( Buff
    , Character
    , Energy(..)
    , Stat(..)
    , addBuff
    , deriveAttack
    , deriveStats
    , hit
    , isAlive
    , new
    , newBuff
    , statString
    , tick
    )

import Cooldown exposing (Cooldown)
import CustomDict as Dict exposing (Dict)


{-| Main Character type
-}
type alias Character =
    { health : ( Int, Int )
    , healthHistory : List Int
    , baseStats : Dict Stat Float
    , buffs : List Buff
    , energy : Dict Energy ( Cooldown, ( Int, Int ) )
    }


{-| Character constructor
-}
new : List ( Stat, Float ) -> Int -> Character
new baseStats health =
    Character
        ( health, health )
        []
        (Dict.fromList baseStats)
        []
        (Dict.fromList
            [ ( Cyan, ( Cooldown.new 1000, ( 0, defaultEnergyCap ) ) )
            , ( Magenta, ( Cooldown.new 1000, ( 0, defaultEnergyCap ) ) )
            , ( Yellow, ( Cooldown.new 1000, ( 0, defaultEnergyCap ) ) )
            ]
        )


{-| tick character buff durations & energy regen.
-}
tick : Float -> Character -> Character
tick dt character =
    if isAlive character then
        { character
            | buffs =
                character.buffs
                    |> List.map (tickBuff dt)
                    |> List.filter buffNotDone
            , energy =
                character.energy
                    |> Dict.map (tickEnergy dt)
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



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0



-- STATS


{-| Holds all stat types
-}
type Stat
    = Attack


{-| stat to string
-}
statString : Stat -> String
statString stat =
    case stat of
        Attack ->
            "🗡️"


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



-- ENERGY


type Energy
    = Cyan
    | Magenta
    | Yellow


defaultEnergyCap : Int
defaultEnergyCap =
    10


tickEnergy : Float -> Energy -> ( Cooldown, ( Int, Int ) ) -> ( Cooldown, ( Int, Int ) )
tickEnergy dt _ ( cooldown, amount ) =
    ( Cooldown.tick dt cooldown, amount )


addEnergy : Energy -> ( Cooldown, ( Int, Int ) ) -> ( Cooldown, ( Int, Int ) )
addEnergy _ ( cooldown, ( amount, cap ) ) =
    if Cooldown.isDone cooldown then
        ( Cooldown.reset cooldown, ( min cap (amount + 1), cap ) )

    else
        ( cooldown, ( amount, cap ) )
