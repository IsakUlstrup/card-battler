module Content.Cards exposing
    ( basicCard
    , basicCard2
    , expensiveCard
    , summonCard
    )

import Card exposing (Card)



-- CARDS


basicCard : Card
basicCard =
    Card.new "Tackle" (Card.Damage 1) 2


basicCard2 : Card
basicCard2 =
    Card.new "Slam" (Card.Damage 2) 3


expensiveCard : Card
expensiveCard =
    Card.new "Fireball" (Card.Damage 10) 7


summonCard : Card
summonCard =
    Card.new "Summon rabbit" (Card.Summon 1) 2
