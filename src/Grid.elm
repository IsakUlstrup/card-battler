module Grid exposing
    ( Grid
    , Point
    , fromList
    , toList
    )

import Dict exposing (Dict)



-- POINT


type alias Point =
    ( Int, Int, Int )


isValid : ( Int, Int, Int ) -> Bool
isValid ( x, y, z ) =
    x + y + z == 0



-- GRID


{-| A hex grid is a dict of points

This is opaque to enforce only valid points

-}
type Grid a
    = Grid (Dict Point a)


{-| Construct grid from list of points.

Ignores invalid points

-}
fromList : List ( Point, a ) -> Grid a
fromList items =
    items
        |> List.filter (Tuple.first >> isValid)
        |> Dict.fromList
        |> Grid


{-| Construct a list of points
-}
toList : Grid a -> List ( Point, a )
toList (Grid grid) =
    Dict.toList grid
