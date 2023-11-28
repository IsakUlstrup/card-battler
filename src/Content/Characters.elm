module Content.Characters exposing (badger, chick, panda, rabbit, unicorn)

import Character exposing (Character)
import Content.Cards
import Stat



-- Players


panda : Character
panda =
    Character.new
        '🐼'
        []
        [ ( Stat.CyanRegenModifier, 2 )
        , ( Stat.YellowRegenModifier, 0.7 )
        , ( Stat.Attack, 2 )
        , ( Stat.Speed, 0.5 )
        ]
        100


unicorn : Character
unicorn =
    Character.new
        '🦄'
        []
        [ ( Stat.CyanRegenModifier, 2 )
        , ( Stat.YellowRegenModifier, 0.7 )
        , ( Stat.CyanCap, 2 )
        , ( Stat.Attack, 5 )
        , ( Stat.Speed, 3 )
        ]
        100



-- Enemies


badger : Character
badger =
    Character.new
        '🦡'
        [ Content.Cards.basicCard, Content.Cards.buffCard ]
        [ ( Stat.CyanRegenModifier, 1 )
        , ( Stat.MagentaRegenModifier, 0.2 )
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
        [ ( Stat.CyanRegenModifier, 0.5 )
        , ( Stat.MagentaRegenModifier, 0.2 )
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
