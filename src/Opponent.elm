module Opponent exposing (Opponent, filterMinion, generateLoot, new, updateMinion)

import Deck exposing (Card)
import Minion exposing (Minion)
import Random exposing (Generator)


type alias Opponent =
    { minion : Minion
    , firstDrop : Maybe ( Float, Card )
    , restDrop : List ( Float, Card )
    }


{-| Create a new opponent, empty drop tables are allowed
-}
new : Minion -> List ( Float, Card ) -> Opponent
new minion dropTable =
    Opponent minion (List.head dropTable) (List.drop 1 dropTable)


updateMinion : (Minion -> Minion) -> Opponent -> Opponent
updateMinion f opponent =
    { opponent | minion = f opponent.minion }


filterMinion : (Minion -> Bool) -> Opponent -> Bool
filterMinion pred opponent =
    pred opponent.minion


generateLoot : Opponent -> Generator (List Card)
generateLoot opponent =
    case opponent.firstDrop of
        Just first ->
            Random.list 3 (Random.weighted first opponent.restDrop)

        Nothing ->
            Random.constant []
