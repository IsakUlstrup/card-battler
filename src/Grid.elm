module Grid exposing
    ( Grid
    , Point
    , fromList
    , pointToAxial
    , pointToString
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


{-| Convert point to string
-}
pointToString : Point -> String
pointToString ( x, y, z ) =
    String.fromInt x
        ++ String.fromInt y
        ++ String.fromInt z


scale : Int -> Point -> Point
scale factor ( x, y, z ) =
    ( x * factor, y * factor, z * factor )


add : Point -> Point -> Point
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + y1, y1 + y2, z1 + z2 )


type alias Directions =
    { one : Point
    , two : Point
    , three : Point
    , four : Point
    , five : Point
    , six : Point
    }


neighbours : Directions
neighbours =
    Directions
        ( 1, 0, -1 )
        ( 1, -1, 0 )
        ( 0, -1, 1 )
        ( -1, 0, 1 )
        ( -1, 1, 0 )
        ( 0, 1, -1 )



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


addRing : Point -> Int -> a -> Grid a -> Grid a
addRing center radius hex (Grid grid) =
    -- function cube_ring(center, radius):
    -- var results = []
    -- # this code doesn't work for radius == 0; can you see why?
    -- var hex = cube_add(center,
    --                     cube_scale(cube_direction(4), radius))
    -- for each 0 ≤ i < 6:
    --     for each 0 ≤ j < radius:
    --         results.append(hex)
    --         hex = cube_neighbor(hex, i)
    -- return results
    let
        h =
            add center (scale radius neighbours.four)
    in
    Grid grid
