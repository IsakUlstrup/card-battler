module Content.Characters exposing (badger, butterfly, chick, panda, rabbit, unicorn)

import Card
import Character exposing (Character)
import Content.Cards as Cards



-- Players


panda : Character
panda =
    Character.new
        '🐼'
        1
        (Card.Damage 2)
        200


unicorn : Character
unicorn =
    Character.new
        '🦄'
        3
        (Card.Damage 4)
        100


butterfly : Character
butterfly =
    Character.new
        '🦋'
        0
        (Card.Damage 1)
        10



-- Enemies


badger : Character
badger =
    Character.new
        '🦡'
        1
        (Card.Damage 2)
        7
        |> Character.setDropTable ( 10, Cards.basicCard ) [ ( 10, Cards.basicCard2 ), ( 10, Cards.expensiveCard ) ]


rabbit : Character
rabbit =
    Character.new
        '🐰'
        5
        (Card.Damage 1)
        5
        |> Character.setDropTable ( 10, Cards.basicCard ) [ ( 40, Cards.basicCard2 ) ]


chick : Character
chick =
    Character.new
        '🐤'
        0
        (Card.Damage 1)
        5
        |> Character.setDropTable ( 10, Cards.basicCard ) []
