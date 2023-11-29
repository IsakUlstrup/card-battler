module Card exposing (Action(..), Card, actionToIcon, actionToString, new)

import Buff exposing (Buff)
import Stat


type Action
    = Attack Int
    | Buff Buff


actionToString : Action -> String
actionToString action =
    case action of
        Attack power ->
            "Attack " ++ String.fromInt power

        Buff buff ->
            "Buff "
                ++ (Stat.toString (Tuple.first buff.statModifier)
                        ++ " x"
                        ++ String.fromFloat (Tuple.second buff.statModifier)
                   )


actionToIcon : Action -> String
actionToIcon action =
    case action of
        Attack power ->
            "🗡️" ++ String.fromInt power

        Buff _ ->
            "buff"


type alias Card =
    { name : String
    , action : Action
    , cost : Int
    }


new : String -> Action -> Int -> Card
new name action cost =
    Card name action cost
