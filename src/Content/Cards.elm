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
import Stat



-- CARDS


basicCard : Card
basicCard =
    Card.new "Basic Attack" (Card.Attack 1) 2


basicCard2 : Card
basicCard2 =
    Card.new "Basic Attack2" (Card.Attack 2) 3


expensiveCard : Card
expensiveCard =
    Card.new "Expensive Attack" (Card.Attack 10) 7


buffCard : Card
buffCard =
    Card.new "Buff yellow regen"
        (Card.Buff (Buff.new 10000 ( Stat.EnergyRegenRate, 3 )))
        2



-- DECKS


testDeck1 : List Card
testDeck1 =
    [ basicCard, basicCard2, expensiveCard, buffCard, basicCard, basicCard2 ]


basicDeck : List Card
basicDeck =
    [ basicCard, basicCard2, basicCard, basicCard2, basicCard, basicCard2 ]
