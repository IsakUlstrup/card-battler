module Render exposing (Config, customSvg, initConfig, viewGrid)

{-| A custom SVG element with camera support
-}

import Grid exposing (Grid, Point)
import Svg exposing (Svg)
import Svg.Attributes



-- CONFIG


{-| Holds renderer config values
-}
type alias Config =
    { cameraPosition : Point
    , cameraZoom : Float
    , hexScale : Float
    , hexFlatTop : Bool
    }


initConfig : Config
initConfig =
    Config ( 0, 0, 0 ) 1 1 True



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


cameraTransform : Config -> Svg.Attribute msg
cameraTransform config =
    let
        ( camX, camY ) =
            config.cameraPosition |> pointToPixel config.hexFlatTop
    in
    Svg.Attributes.style
        ("transform: translate("
            ++ String.fromFloat -(camX * config.cameraZoom)
            ++ "px, "
            ++ String.fromFloat -(camY * config.cameraZoom)
            ++ "px) scale("
            ++ String.fromFloat config.cameraZoom
            ++ ");"
        )


viewBoxAttr : Svg.Attribute msg
viewBoxAttr =
    Svg.Attributes.viewBox
        ([ -500, -500, 1000, 1000 ]
            |> List.map String.fromFloat
            |> List.intersperse " "
            |> String.concat
        )


customSvg : Config -> List (Svg msg) -> Svg msg
customSvg config children =
    Svg.svg
        [ viewBoxAttr
        , Svg.Attributes.preserveAspectRatio "xMidyMid meet"
        , Svg.Attributes.width "1000px"
        , Svg.Attributes.height "1000px"
        , Svg.Attributes.class "hex-svg"
        ]
        [ Svg.g
            [ cameraTransform config
            , Svg.Attributes.class "camera"
            ]
            children
        ]


viewGrid : Grid a -> (( Point, a ) -> Svg msg) -> Svg msg
viewGrid grid viewHex =
    Svg.g [] (grid |> Grid.toList |> List.map viewHex)
