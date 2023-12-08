module Content.Minions exposing (badger, butterfly, chick, fairy, panda, rabbit, turtle, unicorn, vampire, zombie)

import Minion exposing (Minion)


panda : Minion
panda =
    Minion.new
        '🐼'
        200
        1
        2


fairy : Minion
fairy =
    Minion.new
        '🧚'
        200
        1
        2


zombie : Minion
zombie =
    Minion.new
        '🧟'
        200
        1
        2


vampire : Minion
vampire =
    Minion.new
        '🧛'
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


turtle : Minion
turtle =
    Minion.new
        '🐢'
        200
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
        '🐇'
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
