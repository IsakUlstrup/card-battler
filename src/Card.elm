module Card exposing (Action(..), Card, actionToString, new)

import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy)


type Action
    = Attack Int


actionToString : Action -> String
actionToString action =
    case action of
        Attack power ->
            "Attack " ++ String.fromInt power


type alias Card =
    { name : String
    , action : Action
    , cost : Dict Energy Int
    }


new : String -> Action -> List ( Energy, Int ) -> Card
new name action cost =
    Card name action (Dict.fromList cost)
