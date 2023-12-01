module Card exposing (Action(..), Card, actionToIcon, actionToString, new)


type Action
    = Damage Int
    | Summon Int


actionToString : Action -> String
actionToString action =
    case action of
        Damage power ->
            "Damage " ++ String.fromInt power

        Summon _ ->
            "Summon"


actionToIcon : Action -> String
actionToIcon action =
    case action of
        Damage power ->
            "🗡️" ++ String.fromInt power

        Summon _ ->
            "s"


type alias Card =
    { name : String
    , action : Action
    , cost : Int
    }


new : String -> Action -> Int -> Card
new name action cost =
    Card name action cost
