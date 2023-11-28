module Stat exposing (Stat(..), toString)

{-| Holds all stat types
-}


type Stat
    = CyanRegenModifier
    | MagentaRegenModifier
    | YellowRegenModifier
    | AutoPlayFirst
    | CyanCap
    | MagentaCap
    | YellowCap
    | Attack
    | Speed


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

        CyanCap ->
            "Cyan cap"

        MagentaCap ->
            "Magenta cap"

        YellowCap ->
            "Yellow cap"

        Attack ->
            "Attack"

        Speed ->
            "Speed"
