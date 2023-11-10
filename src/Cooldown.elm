module Cooldown exposing (Cooldown, isDone, isStart, new, reset, tick)


type alias Cooldown =
    ( Float, Float )


new : Float -> Cooldown
new duration =
    ( 0, duration )


tick : Float -> Cooldown -> Cooldown
tick dt ( cd, maxCd ) =
    ( min maxCd (cd + dt), maxCd )


isDone : Cooldown -> Bool
isDone ( cd, maxCd ) =
    cd == maxCd


isStart : Cooldown -> Bool
isStart ( cd, _ ) =
    cd == 0


reset : Cooldown -> Cooldown
reset ( _, maxCd ) =
    ( 0, maxCd )
