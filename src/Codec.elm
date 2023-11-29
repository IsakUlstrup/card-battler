module Codec exposing (parseCards, saveCards)

import Card exposing (Action, Card)
import Json.Decode as Decode exposing (Decoder)
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



-- DECODER


decodeAction : Decoder Action
decodeAction =
    Decode.succeed (Card.Damage 1)


decodeCard : Decoder Card
decodeCard =
    Decode.map3 Card.new
        (Decode.field "name" Decode.string)
        (Decode.field "action" decodeAction)
        (Decode.field "cost" Decode.int)


parseCards : Decoder (List Card)
parseCards =
    Decode.list decodeCard
