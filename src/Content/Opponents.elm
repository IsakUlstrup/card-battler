module Content.Opponents exposing (..)

import Content.Cards as Cards
import Content.Minions as Minions
import Opponent exposing (Opponent)


badger : Opponent
badger =
    Opponent.new Minions.badger [ ( 10, Cards.basicCard ) ]


rabbit : Opponent
rabbit =
    Opponent.new Minions.rabbit [ ( 10, Cards.basicCard ), ( 10, Cards.summonCard ) ]


chick : Opponent
chick =
    Opponent.new Minions.chick [ ( 100, Cards.summonTurtle ) ]
