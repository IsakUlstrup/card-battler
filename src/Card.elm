module Card exposing (Action(..), Card, actionToIcon, actionToString, new)

import Buff exposing (Buff)
import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy)
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
    , cost : Dict Energy Int
    }


new : String -> Action -> List ( Energy, Int ) -> Card
new name action cost =
    Card name action (Dict.fromList cost)
