module Stat exposing (Stat(..), toString)

{-| Holds all stat types
-}


type Stat
    = EnergyRegenRate
    | EnergyCap
    | AutoPlayFirst
    | Attack
    | Speed


{-| stat to string
-}
toString : Stat -> String
toString stat =
    case stat of
        EnergyRegenRate ->
            "Energy regen rate"

        EnergyCap ->
            "Energy cap"

        AutoPlayFirst ->
            "Auto play first"

        Attack ->
            "Attack"

        Speed ->
            "Speed"
