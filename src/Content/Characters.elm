module Content.Characters exposing (badger, chick, panda, rabbit, unicorn)

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
        , ( Stat.Attack, 1 )
        , ( Stat.Speed, 0.5 )
        ]
        200


unicorn : Character
unicorn =
    Character.new
        '🦄'
        []
        [ ( Stat.EnergyRegenRate, 2 )
        , ( Stat.EnergyCap, 2 )
        , ( Stat.Attack, 5 )
        , ( Stat.Speed, 3 )
        ]
        100



-- Enemies


badger : Character
badger =
    Character.new
        '🦡'
        []
        [ ( Stat.EnergyRegenRate, 1 )
        , ( Stat.EnergyCap, 2 )
        , ( Stat.AutoPlayFirst, 1 )
        , ( Stat.Attack, 5 )
        , ( Stat.Speed, 2 )
        ]
        7


rabbit : Character
rabbit =
    Character.new
        '🐰'
        []
        [ ( Stat.EnergyRegenRate, 0.5 )
        , ( Stat.AutoPlayFirst, 1 )
        , ( Stat.Attack, 1 )
        , ( Stat.Speed, 5 )
        ]
        5


chick : Character
chick =
    Character.new
        '🐤'
        []
        []
        5
