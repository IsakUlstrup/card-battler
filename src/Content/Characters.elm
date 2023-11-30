module Content.Characters exposing (badger, butterfly, chick, panda, rabbit, unicorn)

import Card
import Character exposing (Character)
import Content.Cards as Cards
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
        Nothing


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
        Nothing


butterfly : Character
butterfly =
    Character.new
        '🦋'
        []
        []
        (Card.Damage 1)
        10
        Nothing



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
        (Just ( ( 10, Cards.basicCard ), [ ( 10, Cards.basicCard2 ), ( 10, Cards.expensiveCard ) ] ))


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
        (Just ( ( 10, Cards.basicCard ), [ ( 40, Cards.basicCard2 ) ] ))


chick : Character
chick =
    Character.new
        '🐤'
        []
        []
        (Card.Damage 1)
        5
        (Just ( ( 10, Cards.basicCard ), [] ))
