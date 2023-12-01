module Content.Opponents exposing (..)

import Content.Cards as Cards
import Content.Minions as Minions
import Deck exposing (Card)
import Minion exposing (Minion)


{-| A minion and a drop table
-}
type alias Opponent =
    ( Minion, ( ( Float, Card ), List ( Float, Card ) ) )


badger : Opponent
badger =
    ( Minions.badger
    , ( ( 10, Cards.basicCard ), [] )
    )


rabbit : Opponent
rabbit =
    ( Minions.rabbit
    , ( ( 10, Cards.basicCard ), [ ( 10, Cards.summonCard ) ] )
    )


chick : Opponent
chick =
    ( Minions.chick
    , ( ( 10, Cards.basicCard ), [] )
    )
