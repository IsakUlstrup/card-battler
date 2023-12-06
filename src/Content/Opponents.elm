module Content.Opponents exposing (..)

import Content.Cards as Cards
import Content.Minions as Minions
import Opponent exposing (Opponent)


badger : Opponent
badger =
    Opponent.new Minions.badger []


rabbit : Opponent
rabbit =
    Opponent.new Minions.rabbit [ ( 50, Cards.basicCard ), ( 50, Cards.summonCard ), ( 1, Cards.summonTurtle ) ]


chick : Opponent
chick =
    Opponent.new Minions.chick []
