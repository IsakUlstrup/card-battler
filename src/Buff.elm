module Buff exposing (Buff, new, notDone, tick)

import Cooldown exposing (Cooldown)
import Stat exposing (Stat)


{-| A buff is a temporary modifier to one stat
-}
type alias Buff =
    { duration : Cooldown
    , statModifier : ( Stat, Float )
    }


{-| Buff constructor
-}
new : Float -> ( Stat, Float ) -> Buff
new duration statModifier =
    Buff (Cooldown.new duration) statModifier


{-| Tick buff duration
-}
tick : Float -> Buff -> Buff
tick dt buff =
    { buff | duration = Cooldown.tick dt buff.duration }


{-| True if buff is not done
-}
notDone : Buff -> Bool
notDone buff =
    Cooldown.isDone buff.duration |> not
