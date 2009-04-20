module Havoc.Negamax where

import Data.List
import Data.Maybe
import Data.Ord
import Havoc.Game
import Havoc.Move
import Havoc.State
import Havoc.Utils

negamax :: (State -> Status) -> (State -> Double) -> (Move -> State -> State) -> Int -> State -> Double
negamax gameStatus evaluate move depth state
    | depth == 0  = heuristicValue
    | otherwise   = case gameStatus state of
                      End _            -> heuristicValue
                      Continue _ moves -> negamaxValue moves
    where
        heuristicValue = evaluate state
        
        recurse = negamax gameStatus evaluate move
        negamaxValue moves = maximum [-(recurse (depth-1) (move m state))
                                     | m <- moves]

negamaxMoves :: (State -> Status) -> (State -> Double) -> (Move -> State -> State) -> Int -> State -> [Move]
negamaxMoves gameStatus evaluate move depth state
    = case gameStatus state of
        End _            -> []
        Continue _ moves -> minimumsBy moveValue moves
    where
        moveValue m = negamax gameStatus evaluate move depth (move m state)
