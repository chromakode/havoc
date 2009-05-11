module Havoc.Player.NegamaxPruned where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Clock
import Havoc.Game
import Havoc.Move
import Havoc.Player.IterativeDeepening
import Havoc.Player.Negamax (negamaxChildNodes)
import Havoc.State
import Havoc.Utils
import Debug.Trace

negamaxPruned :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> Double -> Double -> (Int, Double)
negamaxPruned gameStatus evaluate move state depth ourBest theirBest
    = case negamaxChildNodes status depth of
        Nothing    -> (1, evaluate status)
        Just moves -> runPrune 0 moves (-1) ourBest
    where
        status = gameStatus state

        recurse = negamaxPruned gameStatus evaluate move
        runPrune nodes []        localBest ourBest = (nodes, localBest)
        runPrune nodes (m:moves) localBest ourBest
            = if (localBest' >= theirBest)
                -- This plays out better than our opponent can force us to be. Stop searching here.
                then (nodes', localBest')
                
                -- This is a reasonable move. Keep searching.
                else runPrune nodes' moves localBest' ourBest'
            where
                (rnodes, moveValueNeg) = recurse (move m state) (depth-1) (-theirBest) (-ourBest)
                nodes' = nodes + rnodes
                localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'

negamaxPrunedMove :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> (Int, [Move])
negamaxPrunedMove gameStatus evaluate move state depth
    = case negamaxChildNodes (gameStatus state) depth of
        Nothing    -> (1, [])
        Just moves -> runTopPrune 0 moves (-1) (-1) []
    where
        runTopPrune nodes []        localBest ourBest bestMove = (nodes, bestMove)
        runTopPrune nodes (m:moves) localBest ourBest bestMove
            = if (localBest' > localBest)
                then runTopPrune nodes' moves localBest' ourBest' [m]
                else runTopPrune nodes' moves localBest' ourBest' bestMove
            where
                (snodes, moveValueNeg)  = negamaxPruned gameStatus evaluate move (move m state) (depth-1) (-1) (-ourBest)
                nodes' = nodes + snodes 
                localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'

negamaxPrunedMoveID :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> NominalDiffTime -> State -> IO (Int, Int, [Move])
negamaxPrunedMoveID gameStatus evaluate move seconds state = iterativelyDeepen (negamaxPrunedMove gameStatus evaluate move) seconds state

