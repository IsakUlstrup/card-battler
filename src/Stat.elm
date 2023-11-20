module Stat exposing (Stat(..), toString)

{-| Holds all stat types
-}


type Stat
    = CyanRegenModifier
    | MagentaRegenModifier
    | YellowRegenModifier
    | AutoPlayFirst


{-| stat to string
-}
toString : Stat -> String
toString stat =
    case stat of
        CyanRegenModifier ->
            "Cyan regen rate"

        MagentaRegenModifier ->
            "Magenta regen rate"

        YellowRegenModifier ->
            "Yellow regen rate"

        AutoPlayFirst ->
            "Auto play first"
