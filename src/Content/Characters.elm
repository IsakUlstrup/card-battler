module Content.Characters exposing (badger, panda)

import Card exposing (Card)
import Character exposing (Character)
import Stat


panda : List Card -> Character
panda deck =
    Character.new
        '🐼'
        deck
        [ ( Stat.CyanRegenModifier, 2 )
        , ( Stat.YellowRegenModifier, 0.7 )
        ]
        100


badger : List Card -> Character
badger deck =
    Character.new
        '🦡'
        deck
        [ ( Stat.CyanRegenModifier, 1 )
        , ( Stat.MagentaRegenModifier, 0.2 )
        , ( Stat.AutoPlayFirst, 1 )
        ]
        20
