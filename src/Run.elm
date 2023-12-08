module Run exposing (Run, TurnState(..), advanceTurnState, enemyWipe, filterDeadMinions, playCard, playerWipe, tickDeck, tickMinions, tickTurnState)

import Cooldown exposing (Cooldown)
import Deck exposing (Card, Deck)
import Minion exposing (Minion)
import Opponent exposing (Opponent)
import Random exposing (Seed)


type alias Run =
    { playerCharacter : Minion
    , playerMinions : List Minion
    , opponentCharacter : Opponent
    , opponentMinions : List Minion
    , turnState : TurnState
    , encounters : List Opponent
    , deck : Deck
    , seed : Seed
    }


type TurnState
    = Recovering
    | PlayerAttacking Minion Int Cooldown
    | OpponentAttacking Minion Int Cooldown
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
                , opponentMinions = List.map (Minion.damage dmg) (List.take 1 run.opponentMinions) ++ List.drop 1 run.opponentMinions
            }

        ( newDeck, Just (Deck.Summon minion) ) ->
            { run
                | deck = newDeck
                , playerMinions = minion :: run.playerMinions |> List.take 5
            }

        ( _, Nothing ) ->
            run


tickMinions : Float -> Run -> Run
tickMinions dt run =
    case run.turnState of
        Recovering ->
            { run
                | playerCharacter = Minion.tick dt run.playerCharacter
                , playerMinions = List.map (Minion.tick dt) run.playerMinions
                , opponentCharacter = Opponent.updateMinion (Minion.tick dt) run.opponentCharacter
                , opponentMinions = List.map (Minion.tick dt) run.opponentMinions
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


playerWipe : Run -> Bool
playerWipe run =
    run.playerCharacter |> Minion.isAlive |> not


getReadyPlayerMinion : Run -> ( Run, Maybe Minion )
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
            , Just (Minion.resetCooldown minion)
            )

        Nothing ->
            if Minion.isReady run.playerCharacter then
                ( { run | playerCharacter = Minion.resetCooldown run.playerCharacter }
                , Just (Minion.resetCooldown run.playerCharacter)
                )

            else
                ( run
                , Nothing
                )


getReadyOpposingMinion : Run -> ( Run, Maybe Minion )
getReadyOpposingMinion run =
    let
        getReady =
            run.opponentMinions
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
            ( { run | opponentMinions = List.indexedMap (resetCooldownAtIndex index) run.opponentMinions }
            , Just (Minion.resetCooldown minion)
            )

        Nothing ->
            if Minion.isReady run.opponentCharacter.minion then
                ( { run | opponentCharacter = Opponent.updateMinion Minion.resetCooldown run.opponentCharacter }
                , Just (Minion.resetCooldown run.opponentCharacter.minion)
                )

            else
                ( run
                , Nothing
                )


enemyWipe : Run -> Bool
enemyWipe run =
    case ( run.opponentCharacter.minion |> Minion.isAlive |> not, run.encounters ) of
        ( True, [] ) ->
            True

        _ ->
            False


updateFirstPlayer : (Minion -> Minion) -> Run -> Run
updateFirstPlayer f run =
    case run.playerMinions of
        x :: xs ->
            { run | playerMinions = f x :: xs }

        [] ->
            { run | playerCharacter = f run.playerCharacter }


updateFirstOpponent : (Minion -> Minion) -> Run -> Run
updateFirstOpponent f run =
    case run.opponentMinions of
        x :: xs ->
            { run | opponentMinions = f x :: xs }

        [] ->
            { run | opponentCharacter = Opponent.updateMinion f run.opponentCharacter }


setAttackingState : Run -> Run
setAttackingState run =
    case getReadyPlayerMinion run of
        ( newRun, Just minion ) ->
            { newRun
                | turnState = PlayerAttacking minion (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)
            }

        ( _, Nothing ) ->
            case getReadyOpposingMinion run of
                ( newRun2, Just minion ) ->
                    { newRun2
                        | turnState = OpponentAttacking minion (Tuple.second minion.ability) (Cooldown.new characterAnimationDuration)
                    }

                ( _, Nothing ) ->
                    run


advanceTurnState : Run -> Run
advanceTurnState run =
    case run.turnState of
        Recovering ->
            if Minion.isAlive run.opponentCharacter.minion then
                setAttackingState run

            else
                let
                    ( rewards, seed ) =
                        Random.step (Opponent.generateLoot run.opponentCharacter) run.seed
                in
                if List.isEmpty rewards then
                    nextEncounter run

                else
                    setRewardState rewards { run | seed = seed }

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
                    []
                , opponentCharacter = character
                , encounters = List.drop 1 runState.encounters
                , turnState = Recovering
            }

        Nothing ->
            runState
