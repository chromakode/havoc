module Havoc.Player.Negamax where

import Data.List
import Data.Maybe
import Data.Ord
import Havoc.Game
import Havoc.Move
import Havoc.State
import Havoc.Utils

negamax :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> (Int, Double)
negamax gameStatus evaluate move state depth
    | depth == 0  = heuristicValue
    | otherwise   = case status of
                      End _ _          -> heuristicValue
                      Continue _ moves -> negamaxValue moves
    where
        status = gameStatus state
        heuristicValue = (1, evaluate status)
        
        recurse = negamax gameStatus evaluate move
        
        negamaxValue moves = (sum nodes, maximum negvalues)
                           where (nodes, negvalues) = unzip [recurse (move m state) (depth-1)
                                                            | m <- moves]
                                 values = map negate values

negamaxMoves :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> (Int, [Move])
negamaxMoves gameStatus evaluate move state depth
    = case gameStatus state of
        End _ _          -> (1, [])
        Continue _ moves -> (sum nodes, minimumsPair movevs)
                            where (nodes, movevs) = unzip [(\(n,v) -> (n,(v,m))) $ doNegamax (move m state) depth
                                                          | m <- moves]
    where
        doNegamax = negamax gameStatus evaluate move 

