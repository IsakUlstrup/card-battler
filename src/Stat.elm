module Stat exposing (Stat(..), toString)

{-| Holds all stat types
-}


type Stat
    = CyanRegenModifier
    | MagentaRegenModifier
    | YellowRegenModifier


{-| stat to string
-}
toString : Stat -> String
toString stat =
    case stat of
        CyanRegenModifier ->
            "cyanRegen"

        MagentaRegenModifier ->
            "magentaRegen"

        YellowRegenModifier ->
            "yellowRegen"
