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
import Debug.Trace

negamax :: (Game a) => a s -> STRef s Int -> Int -> ST s Score
negamax state nodeCount depth = do
    modifySTRef nodeCount (+1)
    if depth == 0 then return 0
      else do sts <- showGameState (gameState state)
              --return $! trace (sts) 0
              moves <- moveGen state
              --return $! trace (show (moves) ++ show (turnColor (gameState state))) 0
              negamaxValue moves
    where        
        negamaxValue moves = do
            values <- mapMoves state (\_ s -> negamax s nodeCount (depth-1)) moves
            return 0

negamaxMoves :: (Game a) => a s -> Int -> ST s (Int, [(Score, Move)])
negamaxMoves state depth = do
    status <- gameStatus state
    if depth == 0 then return (0, [])
      else do moves <- moveGen state
              return $! trace (show (moves) ++ show (turnColor (gameState state))) 0
              nodeCount <- newSTRef 1
              movevs <- mapMoves state (\m s -> do v <- negamax s nodeCount (depth-1)
                                                   sts <- showGameState (gameState state)
                                                   --return $! trace (show m ++ sts) 0
                                                   return (v, m)
                                       ) moves
              nodes <- readSTRef nodeCount
              return (trace (show nodes) nodes, minimumsPair movevs)

negamaxMovesID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
negamaxMovesID debugLn seconds state = iterativelyDeepen debugLn (\s d -> stToIO (negamaxMoves s d)) seconds state

