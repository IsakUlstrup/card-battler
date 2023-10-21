module Render exposing (Config, customSvg, initConfig, viewGrid)

{-| A custom SVG element with camera support
-}

import Grid exposing (Grid, Point)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Keyed



-- CONFIG


{-| Holds renderer config values
-}
type alias Config =
    { cameraPosition : Point
    , cameraZoom : Float
    , hexFlatTop : Bool
    }


initConfig : Config
initConfig =
    Config ( 0, 0, 0 ) 1 True



-- VIEW


hexSize : Float
hexSize =
    50


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : Bool -> Point -> ( Float, Float )
pointToPixel flatTop point =
    let
        ( q, r ) =
            Grid.pointToAxial point
    in
    if flatTop then
        ( hexSize * (3 / 2 * toFloat q)
        , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
        )

    else
        ( hexSize * (sqrt 3 * toFloat q + sqrt 3 / 2 * toFloat r)
        , hexSize * (3 / 2 * toFloat r)
        )


{-| Camera transform attribute
-}
cameraTransform : Config -> Svg.Attribute msg
cameraTransform config =
    let
        ( camX, camY ) =
            config.cameraPosition |> pointToPixel config.hexFlatTop
    in
    Svg.Attributes.style
        ("transform: translate("
            ++ String.fromInt -(camX * config.cameraZoom |> round)
            ++ "px, "
            ++ String.fromInt -(camY * config.cameraZoom |> round)
            ++ "px) scale("
            ++ String.fromFloat config.cameraZoom
            ++ ");"
        )


{-| Hex transform attribute
-}
hexTransform : Config -> Point -> Svg.Attribute msg
hexTransform config position =
    let
        ( x, y ) =
            position |> pointToPixel config.hexFlatTop
    in
    Svg.Attributes.style
        ("transform: translate("
            ++ String.fromInt (round x)
            ++ "px, "
            ++ String.fromInt (round y)
            ++ "px)"
        )


{-| Render an svg element with camera support
-}
customSvg : Config -> List (Svg msg) -> Svg msg
customSvg config children =
    Svg.svg
        [ Svg.Attributes.viewBox "-500 -500 1000 1000"
        , Svg.Attributes.preserveAspectRatio "xMidyMid meet"
        , Svg.Attributes.class "hex-svg"
        ]
        [ Svg.g
            [ cameraTransform config
            , Svg.Attributes.class "camera"
            ]
            children
        ]


{-| View a grid
-}
viewGrid : Config -> Grid a -> (( Point, a ) -> Svg msg) -> Svg msg
viewGrid config grid viewHex =
    let
        viewHexWrapper : ( Point, a ) -> ( String, Svg msg )
        viewHexWrapper ( position, hex ) =
            ( Grid.pointToString position, Svg.g [ hexTransform config position, Svg.Attributes.class "tile" ] [ viewHex ( position, hex ) ] )
    in
    Svg.Keyed.node "g" [ Svg.Attributes.class "grid" ] (grid |> Grid.toList |> List.map viewHexWrapper)
