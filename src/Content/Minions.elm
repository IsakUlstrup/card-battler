module Content.Minions exposing (badger, butterfly, chick, panda, rabbit, unicorn)

import Content.Cards as Cards
import Deck
import Minion exposing (Minion)


panda : Minion
panda =
    Minion.new
        '🐼'
        200
        1
        (Deck.Damage 2)


unicorn : Minion
unicorn =
    Minion.new
        '🦄'
        100
        3
        (Deck.Damage 4)


butterfly : Minion
butterfly =
    Minion.new
        '🦋'
        10
        0
        (Deck.Damage 1)


badger : Minion
badger =
    Minion.new
        '🦡'
        7
        1
        (Deck.Damage 2)
        |> Minion.setDroptable ( 10, Cards.basicCard ) [ ( 10, Cards.basicCard2 ), ( 10, Cards.expensiveCard ) ]


rabbit : Minion
rabbit =
    Minion.new
        '🐰'
        5
        5
        (Deck.Damage 1)
        |> Minion.setDroptable ( 10, Cards.basicCard ) [ ( 40, Cards.basicCard2 ), ( 50, Cards.summonCard ) ]


chick : Minion
chick =
    Minion.new
        '🐤'
        5
        0
        (Deck.Damage 1)
        |> Minion.setDroptable ( 10, Cards.basicCard ) []
