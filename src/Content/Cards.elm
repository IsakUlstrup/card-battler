module Content.Cards exposing
    ( basicCard
    , basicCard2
    , basicDeck
    , expensiveCard
    , testDeck1
    )

import Card exposing (Card)



-- CARDS


basicCard : Card
basicCard =
    Card.new "Basic Attack" (Card.Damage 1) 2


basicCard2 : Card
basicCard2 =
    Card.new "Basic Attack2" (Card.Damage 2) 3


expensiveCard : Card
expensiveCard =
    Card.new "Expensive Attack" (Card.Damage 10) 7



-- DECKS


testDeck1 : List Card
testDeck1 =
    [ basicCard, basicCard2, expensiveCard, basicCard, basicCard2 ]


basicDeck : List Card
basicDeck =
    [ basicCard, basicCard2, basicCard, basicCard2, basicCard, basicCard2 ]
