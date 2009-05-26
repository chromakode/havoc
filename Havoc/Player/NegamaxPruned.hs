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

negamaxPruned :: (Game a) => a RealWorld -> GameStatus -> IORef Int -> Int -> Int -> Int -> IO Int
negamaxPruned state status nodeCount depth ourBest theirBest = do
    case negamaxChildNodes status depth of
        Nothing    -> stToIO $ evaluate state status
        Just moves -> do stdGen <- getStdGen
                         statusSorted <- stToIO $ shuffleAndSortStatuses stdGen state moves
                         runPrune statusSorted (-max_eval_score) ourBest
    where
        runPrune [] localBest ourBest = return localBest
        runPrune ((move, state, status):statuses) localBest ourBest = do
            modifyIORef nodeCount (+1)
            moveValueNeg <- negamaxPruned state status nodeCount (depth-1) (-theirBest) (-ourBest)
            
            let localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'
            
            if (localBest' >= theirBest)
                -- This plays out better than our opponent can force us to be. Stop searching here.
                then return localBest'
                
                -- This is a reasonable move. Keep searching.
                else runPrune statuses localBest' ourBest'
                
negamaxPrunedMove :: (Game a) => a RealWorld -> Int -> IO (Int, [(Int, Move)])
negamaxPrunedMove state depth = do
    status <- stToIO $ gameStatus state
    case negamaxChildNodes status depth of
        Nothing    -> return (1, [])
        Just moves -> do nodeCount <- newIORef 1
                         stdGen <- getStdGen
                         statusSorted <- stToIO $ shuffleAndSortStatuses stdGen state moves
                         runTopPrune statusSorted nodeCount (-max_eval_score) (-max_eval_score) []
    where
        status = gameStatus state
    
        runTopPrune [] nodeCount localBest ourBest bestMoves = do nodes <- readIORef nodeCount
                                                                  return (nodes, bestMoves)
        runTopPrune ((move, state, status):statuses) nodeCount localBest ourBest bestMoves = do
            moveValueNeg <- negamaxPruned state status nodeCount (depth-1) (-1) (-ourBest)
            let curValue   = -moveValueNeg
                localBest' = max localBest curValue
                ourBest'   = max ourBest curValue
                bestMoves' = case compare curValue localBest of 
                               GT -> [(curValue, move)]
                               EQ -> (curValue, move):bestMoves
                               LT -> bestMoves
            runTopPrune statuses nodeCount localBest' ourBest' bestMoves'

negamaxPrunedMoveID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
negamaxPrunedMoveID debugLn seconds state = iterativelyDeepen debugLn negamaxPrunedMove seconds state

