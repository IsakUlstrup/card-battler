module Codec exposing (saveCards)

import Card exposing (Action, Card)
import Json.Encode as Encode
import Ports



-- ENCODER


costEncoder : Int -> Encode.Value
costEncoder cost =
    Encode.string (String.fromInt cost)


actionEncoder : Action -> Encode.Value
actionEncoder action =
    Encode.string (Card.actionToString action)


cardEncoder : Card -> Encode.Value
cardEncoder card =
    Encode.object
        [ ( "name", Encode.string card.name )
        , ( "action", actionEncoder card.action )
        , ( "cost", costEncoder card.cost )
        ]


saveCards : List Card -> Cmd msg
saveCards cards =
    Encode.list cardEncoder cards
        |> Encode.encode 0
        |> Ports.storeCards
