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

negamaxChildNodes :: GameStatus -> Int -> Maybe [Move]
negamaxChildNodes status depth
    | depth == 0  = Nothing
    | otherwise   = case status of
                      End _          -> Nothing
                      Continue moves -> Just moves

negamax :: (Game a) => a s -> STRef s Int -> Int -> ST s Int
negamax state nodeCount depth = do
    modifySTRef nodeCount (+1)
    status <- gameStatus state
    case negamaxChildNodes status depth of
        Nothing    -> evaluate state status
        Just moves -> negamaxValue moves
    where        
        negamaxValue moves = do
            values <- mapMoves state (\(_,s) -> negamax s nodeCount (depth-1)) moves
            return $ minimum values

negamaxMoves :: (Game a) => a s -> Int -> ST s (Int, [(Int, Move)])
negamaxMoves state depth = do
    status <- gameStatus state
    case negamaxChildNodes status depth of
        Nothing    -> return (1, [])
        Just moves -> do nodeCount <- newSTRef 1
                         movevs <- mapMoves state (\(m,s) -> do v <- negamax s nodeCount (depth-1) 
                                                                return (v, m)
                                                  ) moves
                         nodes <- readSTRef nodeCount
                         return (nodes, minimumsPair movevs)

negamaxMovesID :: (Game a) => (String -> IO ()) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
negamaxMovesID debugLn seconds state = iterativelyDeepen debugLn (\s d -> stToIO (negamaxMoves s d)) seconds state

