module Suite exposing (cooldown)

import Cooldown
import Expect as Expect
import Fuzz
import Test exposing (Test, test)


cooldown : Test
cooldown =
    Test.describe "Cooldown tests"
        [ Test.fuzz Fuzz.float "Fuzzy constructor, duration should not be negative" <|
            \duration ->
                Cooldown.new duration
                    |> Tuple.second
                    |> Expect.atLeast 0
        , Test.fuzz Fuzz.float "Fuzzy tick, current duration should not be negative" <|
            \deltaTime ->
                Cooldown.new 1000
                    |> Cooldown.tick 200
                    |> Cooldown.tick deltaTime
                    |> Tuple.first
                    |> Expect.atLeast 0
        , Test.fuzz Fuzz.float "Fuzzy tick, current duration should not be greater that max duration" <|
            \deltaTime ->
                Cooldown.new 1000
                    |> Cooldown.tick 200
                    |> Cooldown.tick deltaTime
                    |> Tuple.first
                    |> Expect.atMost 1000
        , test "Cooldown value should not exceed max" <|
            \_ ->
                Cooldown.new 1000
                    |> Cooldown.tick 5000
                    |> Tuple.first
                    |> Expect.equal 1000
        ]
