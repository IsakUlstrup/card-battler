module Content.Characters exposing (badger, butterfly, chick, panda, rabbit, unicorn)

import Card
import Character exposing (Character)
import Stat



-- Players


panda : Character
panda =
    Character.new
        '🐼'
        []
        [ ( Stat.EnergyRegenRate, 5 )
        , ( Stat.EnergyCap, 5 )
        , ( Stat.Speed, 0.5 )
        ]
        (Card.Damage 2)
        200


unicorn : Character
unicorn =
    Character.new
        '🦄'
        []
        [ ( Stat.EnergyRegenRate, 2 )
        , ( Stat.EnergyCap, 2 )
        , ( Stat.Speed, 3 )
        ]
        (Card.Damage 4)
        100


butterfly : Character
butterfly =
    Character.new
        '🦋'
        []
        []
        (Card.Damage 1)
        10



-- Enemies


badger : Character
badger =
    Character.new
        '🦡'
        []
        [ ( Stat.EnergyRegenRate, 1 )
        , ( Stat.EnergyCap, 2 )
        , ( Stat.AutoPlayFirst, 1 )
        , ( Stat.Speed, 2 )
        ]
        (Card.Damage 2)
        7


rabbit : Character
rabbit =
    Character.new
        '🐰'
        []
        [ ( Stat.EnergyRegenRate, 0.5 )
        , ( Stat.AutoPlayFirst, 1 )
        , ( Stat.Speed, 5 )
        ]
        (Card.Damage 1)
        5


chick : Character
chick =
    Character.new
        '🐤'
        []
        []
        (Card.Damage 1)
        5
