module Run exposing (Run, TurnState(..), advanceTurnState, filterDeadMinions, playCard, playerWipe, tickDeck, tickMinions, tickTurnState)

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
    | PlayerAttacking Int Int Cooldown
    | OpponentAttacking Int Int Cooldown
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
                , playerMinions = run.playerMinions ++ [ minion ]
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

        PlayerAttacking index action cooldown ->
            { run | turnState = PlayerAttacking index action (Cooldown.tick dt cooldown) }

        OpponentAttacking index action cooldown ->
            { run | turnState = OpponentAttacking index action (Cooldown.tick dt cooldown) }

        Reward _ ->
            run


filterDeadMinions : Run -> Run
filterDeadMinions run =
    { run | playerMinions = List.filter Minion.isAlive run.playerMinions }


setRecoveringState : Run -> Run
setRecoveringState run =
    { run | turnState = Recovering }


setRewardState : List Card -> Run -> Run
setRewardState rewards run =
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


getReadyPlayerMinion : Run -> ( Run, Maybe ( Int, Minion ) )
getReadyPlayerMinion run =
    let
        getReady =
            run.playerMinions
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, minion ) -> Minion.isReady minion)
                |> List.head

        resetCooldownAtIndex targetIndex index minion =
            if index == targetIndex then
                Minion.resetCooldown minion

            else
                minion
    in
    case getReady of
        Just ( index, minion ) ->
            ( { run | playerMinions = List.indexedMap (resetCooldownAtIndex index) run.playerMinions }
            , Just ( index, minion )
            )

        Nothing ->
            ( run
            , Nothing
            )


getReadyOpposingMinion : Run -> ( Run, Maybe ( Int, Minion ) )
getReadyOpposingMinion run =
    let
        getReady =
            run.opponentMinions
                |> List.map .minion
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, minion ) -> Minion.isReady minion)
                |> List.head

        resetCooldownAtIndex targetIndex index minion =
            if index == targetIndex then
                Opponent.updateMinion Minion.resetCooldown minion

            else
                minion
    in
    case getReady of
        Just ( index, minion ) ->
            ( { run | opponentMinions = List.indexedMap (resetCooldownAtIndex index) run.opponentMinions }
            , Just ( index, minion )
            )

        Nothing ->
            ( run
            , Nothing
            )



-- getReadyOpposingMinion : Run -> Maybe ( Int, Minion )
-- getReadyOpposingMinion run =
--     (run.opponentMinions |> List.map .minion |> List.indexedMap Tuple.pair)
--         |> List.filter (\( _, minion ) -> Minion.isReady minion)
--         |> List.head


updateFirstPlayer : (Minion -> Minion) -> Run -> Run
updateFirstPlayer f run =
    { run | playerMinions = List.map f (List.take 1 run.playerMinions) ++ List.drop 1 run.playerMinions }


updateFirstOpponent : (Minion -> Minion) -> Run -> Run
updateFirstOpponent f run =
    { run | opponentMinions = List.map (Opponent.updateMinion f) (List.take 1 run.opponentMinions) ++ List.drop 1 run.opponentMinions }


setAttackingState : Run -> Run
setAttackingState run =
    case getReadyPlayerMinion run of
        ( newRun, Just ( index, minion ) ) ->
            { newRun
                | turnState = PlayerAttacking index (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)
            }

        ( _, Nothing ) ->
            case getReadyOpposingMinion run of
                ( newRun2, Just ( index, minion ) ) ->
                    { newRun2
                        | turnState = OpponentAttacking index (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)
                    }

                ( _, Nothing ) ->
                    run


advanceTurnState : Run -> Run
advanceTurnState run =
    case run.turnState of
        Recovering ->
            case getDeadOpponent run of
                Just opponent ->
                    let
                        ( rewards, seed ) =
                            Random.step (Opponent.generateLoot opponent) run.seed
                    in
                    if List.isEmpty rewards then
                        nextEncounter run

                    else
                        setRewardState rewards { run | seed = seed }

                Nothing ->
                    setAttackingState run

        PlayerAttacking _ attack cooldown ->
            if Cooldown.isDone cooldown then
                run
                    |> updateFirstOpponent (Minion.damage attack)
                    |> setRecoveringState

            else
                run

        OpponentAttacking _ attack cooldown ->
            if Cooldown.isDone cooldown then
                run
                    |> updateFirstPlayer (Minion.damage attack)
                    |> setRecoveringState

            else
                run

        Reward _ ->
            run


nextEncounter : Run -> Run
nextEncounter runState =
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
