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
import System.Random
import System.Random.Shuffle

shuffleAndSortStatuses :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> Status -> [Move] -> IO [(Move, Status)]
shuffleAndSortStatuses gameStatus evaluate move status@(Continue state _) moves = do
    let statuses = [(m, (gameStatus . move m) state) | m <- moves]
    stdGen <- getStdGen
    let statusShuffled = shuffle' statuses (length statuses) stdGen
        statusSorted   = sortBy (comparing (evaluate . snd)) statusShuffled
    return statusSorted

negamaxPruned :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> Status -> Int -> Double -> Double -> IO (Int, Double)
negamaxPruned gameStatus evaluate move status depth ourBest theirBest = do
    case negamaxChildNodes status depth of
        Nothing    -> return (1, evaluate status)
        Just moves -> do statusSorted <- shuffleAndSortStatuses gameStatus evaluate move status moves
                         runPrune 0 (map snd statusSorted) (-1) ourBest
    where
        recurse = negamaxPruned gameStatus evaluate move
        runPrune nodes []           localBest ourBest = return (nodes, localBest)
        runPrune nodes (s:statuses) localBest ourBest = do
            (rnodes, moveValueNeg) <- recurse s (depth-1) (-theirBest) (-ourBest)
            
            let nodes'     = nodes + rnodes
                localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'
            
            if (localBest' >= theirBest)
                -- This plays out better than our opponent can force us to be. Stop searching here.
                then return (nodes', localBest')
                
                -- This is a reasonable move. Keep searching.
                else runPrune nodes' statuses localBest' ourBest'
                
negamaxPrunedMove :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> IO (Int, [(Double, Move)])
negamaxPrunedMove gameStatus evaluate move state depth = do
    case negamaxChildNodes status depth of
        Nothing    -> return (1, [])
        Just moves -> do statusSorted <- shuffleAndSortStatuses gameStatus evaluate move status moves
                         runTopPrune 0 statusSorted (-1) (-1) []
    where
        status = gameStatus state
    
        runTopPrune nodes []               localBest ourBest bestMoves = return (nodes, bestMoves)
        runTopPrune nodes ((m,s):statuses) localBest ourBest bestMoves = do
            (snodes, moveValueNeg) <- negamaxPruned gameStatus evaluate move s (depth-1) (-1) (-ourBest)
            let nodes'     = nodes + snodes
                curValue   = -moveValueNeg
                localBest' = max localBest curValue
                ourBest'   = max ourBest curValue
                bestMoves' = case compare curValue localBest of 
                               GT -> [(curValue, m)]
                               EQ -> (curValue, m):bestMoves
                               LT -> bestMoves
            runTopPrune nodes' statuses localBest' ourBest' bestMoves'

negamaxPrunedMoveID :: (String -> IO ()) -> (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> NominalDiffTime -> State -> IO (Int, Int, [(Double, Move)])
negamaxPrunedMoveID debugLn gameStatus evaluate move seconds state = iterativelyDeepen debugLn (negamaxPrunedMove gameStatus evaluate move) seconds state

