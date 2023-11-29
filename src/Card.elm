module Card exposing (Action(..), Card, actionToIcon, actionToString, new)


type Action
    = Damage Int


actionToString : Action -> String
actionToString action =
    case action of
        Damage power ->
            "Attack " ++ String.fromInt power


actionToIcon : Action -> String
actionToIcon action =
    case action of
        Damage power ->
            "🗡️" ++ String.fromInt power


type alias Card =
    { name : String
    , action : Action
    , cost : Int
    }


new : String -> Action -> Int -> Card
new name action cost =
    Card name action cost
