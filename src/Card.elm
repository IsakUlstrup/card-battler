module Card exposing (Action(..), Card, new)

import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy)


type Action
    = Attack Int


type alias Card =
    { name : String
    , action : Action
    , cost : Dict Energy Int
    }


new : String -> Action -> List ( Energy, Int ) -> Card
new name action cost =
    Card name action (Dict.fromList cost)
