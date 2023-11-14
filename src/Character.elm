module Character exposing
    ( Character
    , CharacterState
    , advanceState
    , getAttack
    , isAlive
    , isIdle
    , new
    , resolveAttack
    , resolveHit
    , stateIcon
    , stateString
    , tickCooldown
    , tickState
    )

import Cooldown exposing (Cooldown)



-- STATE


{-|

    Character state

-}
type CharacterState
    = Idle
    | Attacking Int Cooldown
    | Hit Int Cooldown


{-|

    Tick state by given delta time

-}
tickCharacterState : Float -> CharacterState -> CharacterState
tickCharacterState dt state =
    case state of
        Idle ->
            Idle

        Attacking attack cd ->
            Attacking attack (Cooldown.tick dt cd)

        Hit hit cd ->
            Hit hit (Cooldown.tick dt cd)


{-| Character state to string. Useful for debugging
-}
stateString : CharacterState -> String
stateString state =
    case state of
        Idle ->
            "idle"

        Attacking _ _ ->
            "attacking"

        Hit _ _ ->
            "hit"


{-| Character state to icon. Useful for debugging
-}
stateIcon : CharacterState -> String
stateIcon state =
    case state of
        Idle ->
            "😴"

        Attacking _ _ ->
            "🗡️"

        Hit _ _ ->
            "🤕"



-- CHARCTER


{-| Main Character type
-}
type alias Character =
    { attack : Int
    , speed : Int
    , health : ( Int, Int )
    , state : CharacterState
    , cooldown : Cooldown
    }


{-| Character constructor
-}
new : Float -> Int -> Int -> Int -> Character
new cooldown attack speed health =
    Character attack speed ( health, health ) Idle (Cooldown.new cooldown)


{-| tick character state by given delta time
-}
tickState : Float -> Character -> Character
tickState dt character =
    { character
        | state = tickCharacterState dt character.state
    }


{-| tick character cooldown by given delta time.

Note: delta time will be multiplied by character speed

-}
tickCooldown : Float -> Character -> Character
tickCooldown dt character =
    if isAlive character then
        { character
            | cooldown = Cooldown.tick (dt * toFloat character.speed) character.cooldown
        }

    else
        character


{-| Advance character state if cooldown is done
-}
advanceState : Character -> Character
advanceState character =
    case character.state of
        Idle ->
            if Cooldown.isDone character.cooldown then
                { character | state = Attacking character.attack (Cooldown.new 1000) }

            else
                character

        Attacking _ cd ->
            if Cooldown.isDone cd then
                { character
                    | state = Idle
                    , cooldown = Cooldown.reset character.cooldown
                }

            else
                character

        Hit _ cd ->
            if Cooldown.isDone cd then
                { character
                    | state = Idle
                    , cooldown = Cooldown.reset character.cooldown
                }

            else
                character


{-| Get character attack, returns Just if character state is attacking and cooldown is done
-}
getAttack : Character -> Maybe Int
getAttack character =
    case character.state of
        Attacking atk cooldown ->
            if Cooldown.isDone cooldown then
                let
                    _ =
                        Debug.log "atk" ()
                in
                Just atk

            else
                Nothing

        _ ->
            Nothing


subtractHp : Int -> Character -> Character
subtractHp amount character =
    { character | health = Tuple.mapFirst (\hp -> max 0 (hp - amount)) character.health }


resolveHit : Int -> Character -> Character
resolveHit hitPower character =
    { character | state = Hit hitPower (Cooldown.new 500) } |> subtractHp hitPower


resolveAttack : Character -> Character
resolveAttack character =
    { character
        | cooldown = Cooldown.reset character.cooldown
        , state = Idle
    }



-- PREDICATES


{-| Is character alive?
-}
isAlive : Character -> Bool
isAlive character =
    Tuple.first character.health > 0


{-| Is character idle?
-}
isIdle : Character -> Bool
isIdle character =
    case character.state of
        Idle ->
            True

        _ ->
            False
