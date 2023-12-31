module Cooldown exposing (Cooldown, isDone, new, reset, tick)


type alias Cooldown =
    ( Float, Float )


new : Float -> Cooldown
new duration =
    ( 0, max 0 duration )


tick : Float -> Cooldown -> Cooldown
tick dt ( cd, maxCd ) =
    ( clamp 0 maxCd (cd + dt), maxCd )


isDone : Cooldown -> Bool
isDone ( cd, maxCd ) =
    cd == maxCd


reset : Cooldown -> Cooldown
reset ( _, maxCd ) =
    ( 0, maxCd )
