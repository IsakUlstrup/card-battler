module Codec exposing (savePosts)

import Card exposing (Action, Card)
import Json.Encode as Encode
import Ports



-- ENCODER


actionEncoder : Action -> Encode.Value
actionEncoder action =
    Encode.string (Card.actionToString action)


cardEncoder : Card -> Encode.Value
cardEncoder card =
    Encode.object
        [ ( "name", Encode.string card.name )
        , ( "action", actionEncoder card.action )
        ]


savePosts : List Card -> Cmd msg
savePosts cards =
    Encode.list cardEncoder cards
        |> Encode.encode 0
        |> Ports.storeCards
