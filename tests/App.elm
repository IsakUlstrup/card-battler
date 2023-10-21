module App exposing (suite)

import Expect
import Fuzz exposing (int)
import Grid
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Grid"
        [ describe "fromList"
            [ test "list of valid points" <|
                \_ ->
                    [ ( ( 0, 0, 0 ), () ) ]
                        |> Grid.fromList
                        |> Grid.toList
                        |> Expect.equal [ ( ( 0, 0, 0 ), () ) ]
            , test "list of invalid points" <|
                \_ ->
                    [ ( ( 0, 0, 1 ), () ) ]
                        |> Grid.fromList
                        |> Grid.toList
                        |> Expect.equal []
            , Test.fuzz3 int int int "insert random point" <|
                \x y z ->
                    let
                        point : Grid.Point
                        point =
                            ( x, y, z )
                    in
                    [ ( point, () ) ]
                        |> Grid.fromList
                        |> Grid.toList
                        |> (\l ->
                                if x + y + z == 0 then
                                    Expect.equal [ ( point, () ) ] l

                                else
                                    Expect.equal [] l
                           )
            ]
        ]
