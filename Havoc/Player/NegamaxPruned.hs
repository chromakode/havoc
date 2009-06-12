module Havoc.Player.NegamaxPruned where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ord
import Data.IORef
import Data.Time.Clock
import Data.Tree
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Globals
import Havoc.Player.DoUndo
import Havoc.Player.IterativeDeepening
import Havoc.Utils
import System.Random
import System.Random.Shuffle

sortMoves :: (Game a) => a s -> [Move] -> ST s [Move]
sortMoves state moves = do
    moves <- mapMoves state (\m s -> do value <- score s
                                        return (value, m)
                            ) moves
    let movesSorted = sortBy (comparing fst) moves
    return $ map snd movesSorted

type MoveSpace = Forest (Evaluated Move)

negamaxPrunedTree :: (Game a) => a RealWorld -> IORef Int -> Int -> Score -> Score -> IO (Score, MoveSpace)
negamaxPrunedTree state nodeCount depth ourBest theirBest = do
    modifyIORef' nodeCount (+1)
    if depth == 0 then (\s -> return (s,[])) =<< (stToIO $ score state)
      else do status <- stToIO $ gameStatus state
              case status of
                End result     -> (\s -> return (s,[])) =<< (stToIO $ evaluateResult state result)
                Continue moves -> do
                    sortedMoves <- stToIO $ sortMoves state moves
                    runPrune sortedMoves ourBest (-max_eval_score) []
    where
        runPrune []           ourBest localBest subForest = return (localBest, subForest)
        runPrune (move:moves) ourBest localBest subForest = do
            
            let recurse = doUndoIO state move (\_ s -> negamaxPrunedTree s nodeCount (depth-1) (-theirBest) (-ourBest))
            (moveValueNeg, subTree) <- if not testDoUndo
                                         then recurse
                                         else checkDoUndoIO state recurse
            
            let localBest' = max localBest (-moveValueNeg)
                ourBest'   = max ourBest localBest'
                node       = Node (Evaluated moveValueNeg move) subTree
                subForest' = node:subForest
            
            if (localBest' >= theirBest)
                -- This plays out better than our opponent can force us to be. Stop searching here.
                then return (localBest', subForest')
                
                -- This is a reasonable move. Keep searching.
                else runPrune moves ourBest' localBest' subForest'

negamaxPruned :: (Game a) => a RealWorld -> IORef Int -> Int -> Score -> Score -> IO Score
negamaxPruned state nodeCount depth ourBest theirBest = do
    (score, movespace) <- negamaxPrunedTree state nodeCount depth ourBest theirBest
    return score
                
negamaxPrunedMoves :: (Game a) => a RealWorld -> Int -> IO (Int, [(Score, Move)])
negamaxPrunedMoves state depth = do
    if depth == 0 then return (0, [])
      else do status <- stToIO $ gameStatus state
              case status of
                End result     -> return (1, [])
                Continue moves -> do
                    stdGen <- newStdGen
                    let shuffledMoves = shuffle' moves (length moves) stdGen
                    sortedMoves <- stToIO $ sortMoves state shuffledMoves
                                        
                    nodeCount <- newIORef 1
                    runTopPrune sortedMoves nodeCount (-max_eval_score) (-max_eval_score) []
    where
        status = gameStatus state
    
        runTopPrune []           nodeCount localBest ourBest bestMoves = do nodes <- readIORef nodeCount
                                                                            return (nodes, bestMoves)
        runTopPrune (move:moves) nodeCount localBest ourBest bestMoves = do
            moveValueNeg <- doUndoIO state move (\_ s -> negamaxPruned s nodeCount (depth-1) (-max_eval_score) (-ourBest))
            let curValue   = -moveValueNeg
                localBest' = max localBest curValue
                ourBest'   = max ourBest curValue
                bestMoves' = if curValue > localBest
                               then [(curValue, move)]
                               else bestMoves
            runTopPrune moves nodeCount localBest' ourBest' bestMoves'

negamaxPrunedMovesID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
negamaxPrunedMovesID debugLn seconds state = iterativelyDeepen debugLn negamaxPrunedMoves seconds state

