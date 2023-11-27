module Content.Cards exposing
    ( basicCard
    , basicCard2
    , basicDeck
    , buffCard
    , expensiveCard
    , testDeck1
    )

import Buff
import Card exposing (Card)
import Energy
import Stat



-- CARDS


basicCard : Card
basicCard =
    Card.new "Basic Attack" (Card.Attack 1) [ ( Energy.Cyan, 2 ) ]


basicCard2 : Card
basicCard2 =
    Card.new "Basic Attack2" (Card.Attack 2) [ ( Energy.Cyan, 4 ) ]


expensiveCard : Card
expensiveCard =
    Card.new "Expensive Attack" (Card.Attack 10) [ ( Energy.Cyan, 8 ), ( Energy.Yellow, 7 ) ]


buffCard : Card
buffCard =
    Card.new "Buff yellow regen"
        (Card.Buff (Buff.new 10000 ( Stat.YellowRegenModifier, 3 )))
        [ ( Energy.Yellow, 2 ) ]



-- DECKS


testDeck1 : List Card
testDeck1 =
    [ basicCard, basicCard2, expensiveCard, buffCard, basicCard, basicCard2 ]


basicDeck : List Card
basicDeck =
    [ basicCard, basicCard2, basicCard, basicCard2, basicCard, basicCard2 ]
