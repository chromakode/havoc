module Havoc.Player.Negamax where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ord
import Data.STRef
import Data.Time
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Player.DoUndo
import Havoc.Player.IterativeDeepening
import Havoc.Utils

negamax :: (Game a) => a s -> STRef s Int -> Int -> ST s Score
negamax state nodeCount depth = do
    modifySTRef nodeCount (+1)
    if depth == 0 then score state
      else do status <- gameStatus state
              case status of
                End result     -> evaluateResult state result
                Continue moves -> negamaxValue moves
    where        
        negamaxValue moves = do
            values <- mapMoves state (\_ s -> negamax s nodeCount (depth-1)) moves
            return $ (negate . minimum) values

negamaxMoves :: (Game a) => a s -> Int -> ST s (Int, [(Score, Move)])
negamaxMoves state depth = do
    status <- gameStatus state
    if depth == 0 then return (0, [])
      else do status <- gameStatus state
              case status of
                End result     -> return (1, [])
                Continue moves -> do
                    nodeCount <- newSTRef 1
                    movevs <- mapMoves state (\m s -> do v <- negamax s nodeCount (depth-1)
                                                         return (v, m)
                                             ) moves
                    nodes <- readSTRef nodeCount
                    return (nodes, minimumsPair movevs)

negamaxMovesID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
negamaxMovesID debugLn seconds state = iterativelyDeepen debugLn (\s d -> stToIO (negamaxMoves s d)) seconds state

