module Havoc.Player.NegamaxPruned where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ord
import Data.STRef
import Data.Time.Clock
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Player.DoUndo
import Havoc.Player.IterativeDeepening
import Havoc.Player.Negamax (negamaxChildNodes)
import Havoc.Utils
import System.Random
import System.Random.Shuffle

shuffleAndSortStatuses :: (Game a) => StdGen -> a s -> [Move] -> ST s [(Move, a s, GameStatus)]
shuffleAndSortStatuses stdGen state moves = do
    statuses <- mapMoves state (\(m,s) -> do status <- gameStatus s
                                             value  <- evaluate s status
                                             return (value, (m, s, status))
                               ) moves
    let statusShuffled = shuffle' statuses (length statuses) stdGen
    
    
        statusSorted   = sortBy (comparing fst) statusShuffled
    return $ map snd statusSorted

negamaxPruned :: (Game a) => a RealWorld -> GameStatus -> STRef RealWorld Int -> Int -> Int -> Int -> IO Int
negamaxPruned state status nodeCount depth ourBest theirBest = do
    case negamaxChildNodes status depth of
        Nothing    -> stToIO $ evaluate state status
        Just moves -> do statusSorted <- shuffleAndSortStatuses status moves
                         runPrune 0 (map snd statusSorted) (-max_eval_score) ourBest
    where
        runPrune nodes []           localBest ourBest = return (nodes, localBest)
        runPrune nodes (s:statuses) localBest ourBest = do
            (rnodes, moveValueNeg) <- negamaxPruned s (depth-1) (-theirBest) (-ourBest)
            
            let nodes'     = nodes + rnodes
                localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'
            
            if (localBest' >= theirBest)
                -- This plays out better than our opponent can force us to be. Stop searching here.
                then return (nodes', localBest')
                
                -- This is a reasonable move. Keep searching.
                else runPrune nodes' statuses localBest' ourBest'
                
negamaxPrunedMove :: (Game a) => a s -> Int -> IO (Int, [(Int, Move)])
negamaxPrunedMove state depth = do
    case negamaxChildNodes status depth of
        Nothing    -> return (1, [])
        Just moves -> do statusSorted <- shuffleAndSortStatuses status moves
                         runTopPrune 0 statusSorted (-max_eval_score) (-max_eval_score) []
    where
        status = gameStatus state
    
        runTopPrune nodes []               localBest ourBest bestMoves = return (nodes, bestMoves)
        runTopPrune nodes ((m,s):statuses) localBest ourBest bestMoves = do
            (snodes, moveValueNeg) <- negamaxPruned s (depth-1) (-1) (-ourBest)
            let nodes'     = nodes + snodes
                curValue   = -moveValueNeg
                localBest' = max localBest curValue
                ourBest'   = max ourBest curValue
                bestMoves' = case compare curValue localBest of 
                               GT -> [(curValue, m)]
                               EQ -> (curValue, m):bestMoves
                               LT -> bestMoves
            runTopPrune nodes' statuses localBest' ourBest' bestMoves'

negamaxPrunedMoveID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a s -> IO (Int, Int, [(Int, Move)])
negamaxPrunedMoveID debugLn seconds state = iterativelyDeepen debugLn negamaxPrunedMove seconds state

