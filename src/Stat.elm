module Stat exposing (Stat(..), toString)

{-| Holds all stat types
-}


type Stat
    = Attack
    | CyanRegenModifier
    | MagentaRegenModifier
    | YellowRegenModifier


{-| stat to string
-}
toString : Stat -> String
toString stat =
    case stat of
        Attack ->
            "attack"

        CyanRegenModifier ->
            "cyanRegen"

        MagentaRegenModifier ->
            "magentaRegen"

        YellowRegenModifier ->
            "yellowRegen"
