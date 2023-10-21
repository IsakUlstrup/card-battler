module Grid exposing
    ( Grid
    , Point
    , fromList
    , pointToAxial
    , toList
    )

import Dict exposing (Dict)



-- POINT


type alias Point =
    ( Int, Int, Int )


isValid : ( Int, Int, Int ) -> Bool
isValid ( x, y, z ) =
    x + y + z == 0


{-| Convert cube point to axial point
Note: returns (0, 0) if point is invalid
-}
pointToAxial : Point -> ( Int, Int )
pointToAxial ( x, y, z ) =
    if isValid ( x, y, z ) then
        ( x, z )

    else
        ( 0, 0 )



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
