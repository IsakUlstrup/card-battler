module Grid exposing
    ( Grid
    , Point
    , circle
    , fromList
    , get
    , insert
    , keys
    , map
    , pointToAxial
    , pointToString
    , randomCircle
    , toList
    , update
    )

import Dict exposing (Dict)
import Random exposing (Generator, Seed)



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
    "("
        ++ String.fromInt x
        ++ ", "
        ++ String.fromInt y
        ++ ", "
        ++ String.fromInt z
        ++ ")"


scale : Int -> Point -> Point
scale factor ( x, y, z ) =
    ( x * factor, y * factor, z * factor )


add : Point -> Point -> Point
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )



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


keys : Grid a -> List Point
keys (Grid grid) =
    Dict.keys grid


insert : Point -> a -> Grid a -> Grid a
insert position tile (Grid grid) =
    Dict.insert position tile grid |> Grid


get : Point -> Grid a -> Maybe a
get position (Grid grid) =
    Dict.get position grid


map : (Point -> a -> b) -> Grid a -> Grid b
map f (Grid grid) =
    Grid (Dict.map f grid)


update : (a -> a) -> Point -> Grid a -> Grid a
update f position (Grid grid) =
    Grid (Dict.update position (\v -> Maybe.map f v) grid)


{-| Get direction given hex side
-}
direction : Int -> Point
direction dir =
    case dir of
        0 ->
            ( 1, -1, 0 )

        1 ->
            ( 1, 0, -1 )

        2 ->
            ( 0, 1, -1 )

        3 ->
            ( -1, 1, 0 )

        4 ->
            ( -1, 0, 1 )

        _ ->
            ( 0, -1, 1 )



-- GENERATORS


{-| Returns a ring of points around given point with given radius
-}
ring : Int -> Point -> List Point
ring radius center =
    let
        start : Int -> Point
        start index =
            add center (scale radius (direction (modBy 6 (index + 4))))

        side : Int -> List Point
        side index =
            List.map (\i -> add (start index) (scale i (direction index))) (List.range 0 radius)
    in
    List.concatMap side (List.range 0 5)


{-| Returns a circle around center of all points within given radius
-}
circle : Int -> Point -> List Point
circle radius center =
    if isValid center then
        List.range 0 radius |> List.concatMap (\r -> ring r center)

    else
        []


{-| Generate a maybe tiles based on emptyChance percentage
-}
randomTile : Float -> a -> Generator (Maybe a)
randomTile emptyChance tile =
    let
        clampedChance =
            clamp 0 100 emptyChance
    in
    Random.weighted ( 100 - clampedChance, Just tile ) [ ( clampedChance, Nothing ) ]


{-| Generate a circle with some tiles missing
-}
randomCircle : Int -> Point -> a -> Seed -> ( Seed, List ( Point, a ) )
randomCircle radius center tile seed =
    let
        helper position ( s, accum ) =
            let
                ( maybeTile, newSeed ) =
                    Random.step (randomTile 30 tile) s
            in
            case maybeTile of
                Just justTile ->
                    ( newSeed, ( position, justTile ) :: accum )

                Nothing ->
                    ( newSeed, accum )
    in
    List.foldl helper ( seed, [] ) (circle radius center)
