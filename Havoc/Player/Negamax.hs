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

negamax :: GameState s -> Int -> STRef s Int -> ST s Int
negamax state depth nodeCount = do
    modifySTRef nodeCount (+1)
    status <- gameStatus state
    case negamaxChildNodes status depth of
        Nothing    -> (1, evaluate status)
        Just moves -> negamaxValue moves
    where        
        negamaxValue moves = do
            values <- mapMoves (\s -> -(negamax s (depth-1) nodeCount)) moves
            return $ maximum values

negamaxMoves :: GameState s -> Int -> (Int, [(Int, Move)])
negamaxMoves state depth
    = case negamaxChildNodes (gameStatus state) depth of
        Nothing    -> (1, [])
        Just moves -> (sum nodes, minimumsPair movevs)
                      where (nodes, movevs) = unzip [(\(n,v) -> (n,(v,m))) $ doNegamax (move m state) depth
                                                    | m <- moves]
                            doNegamax = negamax gameStatus evaluate move


negamaxMovesID :: (String -> IO ()) -> NominalDiffTime -> GameState s -> IO (Int, Int, [(Int, Move)])
negamaxMovesID debugLn seconds state = iterativelyDeepen debugLn (\s d -> return (negamaxMoves s d)) seconds state

