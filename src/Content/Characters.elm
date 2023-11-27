module Content.Characters exposing (badger, chick, panda, rabbit)

import Character exposing (Character)
import Stat


panda : Character
panda =
    Character.new
        '🐼'
        []
        [ ( Stat.CyanRegenModifier, 2 )
        , ( Stat.YellowRegenModifier, 0.7 )
        ]
        100


badger : Character
badger =
    Character.new
        '🦡'
        []
        [ ( Stat.CyanRegenModifier, 1 )
        , ( Stat.MagentaRegenModifier, 0.2 )
        , ( Stat.AutoPlayFirst, 1 )
        ]
        20


rabbit : Character
rabbit =
    Character.new
        '🐰'
        []
        [ ( Stat.CyanRegenModifier, 0.5 )
        , ( Stat.MagentaRegenModifier, 0.2 )
        , ( Stat.AutoPlayFirst, 1 )
        ]
        5


chick : Character
chick =
    Character.new
        '🐤'
        []
        [ ( Stat.CyanRegenModifier, 0.5 )
        , ( Stat.MagentaRegenModifier, 0.2 )
        , ( Stat.AutoPlayFirst, 1 )
        ]
        5
