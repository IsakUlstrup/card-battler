module Codec exposing (saveCards)

import Card exposing (Action, Card)
import CustomDict as Dict exposing (Dict)
import Energy exposing (Energy)
import Json.Encode as Encode
import Ports



-- ENCODER


energyEncoder : ( Energy, Int ) -> Encode.Value
energyEncoder ( energy, amount ) =
    Encode.object
        [ ( "first", Encode.string (Energy.toString energy) )
        , ( "second", Encode.string (String.fromInt amount) )
        ]


costEncoder : Dict Energy Int -> Encode.Value
costEncoder cost =
    Encode.list energyEncoder (Dict.toList cost)


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
