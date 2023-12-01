module Content.Minions exposing (badger, butterfly, chick, panda, rabbit, unicorn)

import Minion exposing (Minion)


panda : Minion
panda =
    Minion.new
        '🐼'
        200
        1
        2


unicorn : Minion
unicorn =
    Minion.new
        '🦄'
        100
        3
        4


butterfly : Minion
butterfly =
    Minion.new
        '🦋'
        10
        0
        1


badger : Minion
badger =
    Minion.new
        '🦡'
        7
        1
        2


rabbit : Minion
rabbit =
    Minion.new
        '🐰'
        5
        5
        1


chick : Minion
chick =
    Minion.new
        '🐤'
        5
        0
        1
