module Havoc.Player.NegamaxPruned where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ord
import Data.IORef
import Data.Time.Clock
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Globals
import Havoc.Player.DoUndo
import Havoc.Player.IterativeDeepening
import Havoc.Utils

sortMoves :: (Game a) => a s -> [Move] -> ST s [Move]
sortMoves state moves = do
    moves <- mapMoves state (\m s -> do value <- score s
                                        return (value, m)
                            ) moves
    let movesSorted = sortBy (comparing fst) moves
    return $ map snd movesSorted

negamaxPruned :: (Game a) => a RealWorld -> IORef Int -> Int -> Score -> Score -> IO Score
negamaxPruned state nodeCount depth ourBest theirBest = do
    modifyIORef' nodeCount (+1)
    if depth == 0 then stToIO $ score state
      else do status <- stToIO $ gameStatus state
              case status of
                End result     -> stToIO $ evaluateResult state result
                Continue moves -> do
                    sortedMoves <- stToIO $ sortMoves state moves
                    runPrune sortedMoves (-max_eval_score) ourBest
    where
        runPrune [] localBest ourBest = return localBest
        runPrune (move:moves) localBest ourBest = do
            
            let recurse = doUndoIO state move (\_ s -> negamaxPruned s nodeCount (depth-1) (-theirBest) (-ourBest))
            moveValueNeg <- if not testDoUndo
                                then recurse
                                else checkDoUndoIO state recurse
            
            let localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'
            
            if (localBest' >= theirBest)
                -- This plays out better than our opponent can force us to be. Stop searching here.
                then return localBest'
                
                -- This is a reasonable move. Keep searching.
                else runPrune moves localBest' ourBest'
                
negamaxPrunedMoves :: (Game a) => a RealWorld -> Int -> IO (Int, [(Score, Move)])
negamaxPrunedMoves state depth = do
    if depth == 0 then return (0, [])
      else do status <- stToIO $ gameStatus state
              case status of
                End result     -> return (1, [])
                Continue moves -> do
                    sortedMoves <- stToIO $ sortMoves state moves
                    
                    nodeCount <- newIORef 1
                    runTopPrune sortedMoves nodeCount (-max_eval_score) (-max_eval_score) []
    where
        status = gameStatus state
    
        runTopPrune [] nodeCount localBest ourBest bestMoves = do nodes <- readIORef nodeCount
                                                                  return (nodes, bestMoves)
        runTopPrune (move:moves) nodeCount localBest ourBest bestMoves = do
            moveValueNeg <- doUndoIO state move (\_ s -> negamaxPruned s nodeCount (depth-1) (-max_eval_score) (-ourBest))
            let curValue   = -moveValueNeg
                localBest' = max localBest curValue
                ourBest'   = max ourBest curValue
                bestMoves' = case compare curValue localBest of 
                               GT -> [(curValue, move)]
                               EQ -> (curValue, move):bestMoves
                               LT -> bestMoves
            runTopPrune moves nodeCount localBest' ourBest' bestMoves'

negamaxPrunedMovesID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
negamaxPrunedMovesID debugLn seconds state = iterativelyDeepen debugLn negamaxPrunedMoves seconds state

