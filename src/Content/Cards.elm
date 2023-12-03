module Content.Cards exposing
    ( basicCard
    , basicCard2
    , expensiveCard
    , summonCard
    , summonTurtle
    )

import Content.Minions as Minions
import Deck exposing (Card)



-- CARDS


basicCard : Card
basicCard =
    Deck.newCard "Tackle" (Deck.Damage 1) 2


basicCard2 : Card
basicCard2 =
    Deck.newCard "Slam" (Deck.Damage 2) 3


expensiveCard : Card
expensiveCard =
    Deck.newCard "Fireball" (Deck.Damage 10) 7


summonCard : Card
summonCard =
    Deck.newCard "Summon rabbit" (Deck.Summon Minions.rabbit) 2


summonTurtle : Card
summonTurtle =
    Deck.newCard "Summon Turtle" (Deck.Summon Minions.turtle) 3
