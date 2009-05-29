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
import Havoc.Player.DoUndo
import Havoc.Player.IterativeDeepening
import Havoc.Utils
import System.Random
import System.Random.Shuffle

shuffleAndSortStatuses :: (Game a) => StdGen -> a s -> [Move] -> ST s [Move]
shuffleAndSortStatuses stdGen state moves = do
    moves <- mapMoves state (\m s -> do value <- score s
                                        return (value, m)
                            ) moves
    let movesShuffled = shuffle' moves (length moves) stdGen
        movesSorted   = sortBy (comparing fst) movesShuffled
    return $ map snd movesSorted

negamaxPruned :: (Game a) => a RealWorld -> IORef Int -> Int -> Score -> Score -> IO Score
negamaxPruned state nodeCount depth ourBest theirBest = do
    if depth == 0 then stToIO $ score state
      else do status <- stToIO $ gameStatus state
              case status of
                End result     -> stToIO $ evaluateResult state result
                Continue moves -> do
                    stdGen <- getStdGen
                    statusSorted <- stToIO $ shuffleAndSortStatuses stdGen state moves
                    runPrune statusSorted (-max_eval_score) ourBest
    where
        runPrune [] localBest ourBest = return localBest
        runPrune (move:moves) localBest ourBest = do
            modifyIORef nodeCount (+1)
            
            moveValueNeg <- doUndoIO state move (\_ s -> negamaxPruned s nodeCount (depth-1) (-theirBest) (-ourBest))
            
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
                    nodeCount <- newIORef 1
                    stdGen <- getStdGen
                    statusSorted <- stToIO $ shuffleAndSortStatuses stdGen state moves
                    runTopPrune statusSorted nodeCount (-max_eval_score) (-max_eval_score) []
    where
        status = gameStatus state
    
        runTopPrune [] nodeCount localBest ourBest bestMoves = do nodes <- readIORef nodeCount
                                                                  return (nodes, bestMoves)
        runTopPrune (move:moves) nodeCount localBest ourBest bestMoves = do
            moveValueNeg <- doUndoIO state move (\_ s -> negamaxPruned s nodeCount (depth-1) (-1) (-ourBest))
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

