module Run exposing (Run, TurnState(..), advanceTurnState, filterDeadMinions, playCard, playerWipe, resetDoneCooldowns, tickDeck, tickMinions, tickTurnState)

import Cooldown exposing (Cooldown)
import Deck exposing (Card, Deck)
import Minion exposing (Minion)
import Opponent exposing (Opponent)
import Random exposing (Seed)


type alias Run =
    { playerMinions : List Minion
    , opponentMinions : List Opponent
    , turnState : TurnState
    , encounters : List Opponent
    , deck : Deck
    , seed : Seed
    }


type TurnState
    = Recovering
    | Attacking Bool Int Int Cooldown
    | Reward (List Card)


characterAnimationDuration : Float
characterAnimationDuration =
    150


playCard : Int -> Run -> Run
playCard index run =
    case Deck.playCardAtIndex index run.deck of
        ( newDeck, Just (Deck.Damage dmg) ) ->
            { run
                | deck = newDeck
                , opponentMinions = List.map (Opponent.updateMinion (Minion.damage dmg)) (List.take 1 run.opponentMinions) ++ List.drop 1 run.opponentMinions
            }

        ( newDeck, Just (Deck.Summon minion) ) ->
            { run
                | deck = newDeck
                , playerMinions = minion :: run.playerMinions
            }

        ( _, Nothing ) ->
            run


tickMinions : Float -> Run -> Run
tickMinions dt run =
    case run.turnState of
        Recovering ->
            { run
                | playerMinions = List.map (Minion.tick dt) run.playerMinions
                , opponentMinions = List.map (Opponent.updateMinion (Minion.tick dt)) run.opponentMinions
            }

        _ ->
            run


tickDeck : Float -> Run -> Run
tickDeck dt run =
    case run.turnState of
        Recovering ->
            { run | deck = Deck.tickEnergy dt run.deck }

        _ ->
            run


tickTurnState : Float -> Run -> Run
tickTurnState dt run =
    case run.turnState of
        Recovering ->
            run

        Attacking isPlayer index action cooldown ->
            { run | turnState = Attacking isPlayer index action (Cooldown.tick dt cooldown) }

        Reward _ ->
            run


filterDeadMinions : Run -> Run
filterDeadMinions run =
    { run | playerMinions = List.filter Minion.isAlive run.playerMinions }


setRecoveringState : Run -> Run
setRecoveringState run =
    { run | turnState = Recovering }


setVictoryState : List Card -> Run -> Run
setVictoryState rewards run =
    { run | turnState = Reward rewards }


getDeadOpponent : Run -> Maybe Opponent
getDeadOpponent run =
    run.opponentMinions
        |> List.filter (Opponent.filterMinion (Minion.isAlive >> not))
        |> List.head


playerWipe : Run -> Bool
playerWipe run =
    (run.playerMinions
        |> List.all (Minion.isAlive >> not)
    )
        || List.isEmpty run.playerMinions


getReadyMinion : Run -> Maybe ( Bool, Minion )
getReadyMinion run =
    (run.playerMinions |> List.map (Tuple.pair True))
        ++ (run.opponentMinions |> List.map .minion |> List.map (Tuple.pair False))
        |> List.filter (\( _, minion ) -> Minion.isReady minion)
        |> List.head


resetDoneCooldowns : Run -> Run
resetDoneCooldowns run =
    let
        resetIfDone minion =
            if Minion.isReady minion then
                Minion.resetCooldown minion

            else
                minion
    in
    { run
        | playerMinions = List.map resetIfDone run.playerMinions
        , opponentMinions = List.map (Opponent.updateMinion resetIfDone) run.opponentMinions
    }


advanceTurnState : Run -> Run
advanceTurnState model =
    case model.turnState of
        Recovering ->
            case getDeadOpponent model of
                Just opponent ->
                    let
                        ( rewards, seed ) =
                            Random.step (Opponent.generateLoot opponent) model.seed
                    in
                    if List.isEmpty rewards then
                        nextEncounter2 model

                    else
                        setVictoryState rewards { model | seed = seed }

                _ ->
                    case getReadyMinion model of
                        Just ( True, minion ) ->
                            { model
                                | turnState = Attacking True 0 (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)

                                -- , playerMinions = List.map Minion.resetCooldown model.playerMinions
                            }

                        Just ( False, minion ) ->
                            { model
                                | turnState = Attacking False 0 (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)

                                -- , opponentMinions = List.map (Opponent.updateMinion Minion.resetCooldown) model.opponentMinions
                            }

                        _ ->
                            model

        Attacking isPlayer _ attack cooldown ->
            if Cooldown.isDone cooldown then
                (if isPlayer then
                    { model | opponentMinions = List.map (Opponent.updateMinion (Minion.damage attack)) (List.take 1 model.opponentMinions) ++ List.drop 1 model.opponentMinions }

                 else
                    { model | playerMinions = List.map (Minion.damage attack) (List.take 1 model.playerMinions) ++ List.drop 1 model.playerMinions }
                )
                    |> setRecoveringState

            else
                model

        Reward _ ->
            model


nextEncounter2 : Run -> Run
nextEncounter2 runState =
    case List.head runState.encounters of
        Just character ->
            { runState
                | playerMinions = List.map Minion.resetCooldown runState.playerMinions
                , opponentMinions =
                    character
                        :: runState.opponentMinions
                        |> List.filter (.minion >> Minion.isAlive)
                , encounters = List.drop 1 runState.encounters
                , turnState = Recovering
            }

        Nothing ->
            runState
