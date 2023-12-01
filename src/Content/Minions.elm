module Content.Minions exposing (badger, butterfly, chick, panda, rabbit, unicorn)

import Content.Cards as Cards
import Deck exposing (Card)
import Minion exposing (Minion)


panda : Minion Card
panda =
    Minion.new
        '🐼'
        200
        1
        2


unicorn : Minion Card
unicorn =
    Minion.new
        '🦄'
        100
        3
        4


butterfly : Minion Card
butterfly =
    Minion.new
        '🦋'
        10
        0
        1


badger : Minion Card
badger =
    Minion.new
        '🦡'
        7
        1
        2
        |> Minion.setDroptable ( 10, Cards.basicCard ) [ ( 10, Cards.basicCard2 ), ( 10, Cards.expensiveCard ) ]


rabbit : Minion Card
rabbit =
    Minion.new
        '🐰'
        5
        5
        1
        |> Minion.setDroptable ( 10, Cards.basicCard ) [ ( 40, Cards.basicCard2 ), ( 50, Cards.summonCard ) ]


chick : Minion Card
chick =
    Minion.new
        '🐤'
        5
        0
        1
        |> Minion.setDroptable ( 10, Cards.basicCard ) []
