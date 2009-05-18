module Havoc.Player.Negamax where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import Havoc.Game
import Havoc.Move
import Havoc.Player.IterativeDeepening
import Havoc.State
import Havoc.Utils

negamaxChildNodes :: Status -> Int -> Maybe [Move]
negamaxChildNodes status depth
    | depth == 0  = Nothing
    | otherwise   = case status of
                      End _ _          -> Nothing
                      Continue _ moves -> Just moves

negamax :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> (Int, Double)
negamax gameStatus evaluate move state depth
    = case negamaxChildNodes status depth of
        Nothing    -> (1, evaluate status)
        Just moves -> negamaxValue moves
    where
        status = gameStatus state
        
        recurse = negamax gameStatus evaluate move
        negamaxValue moves = (sum nodes, maximum negvalues)
                           where (nodes, negvalues) = unzip [recurse (move m state) (depth-1)
                                                            | m <- moves]
                                 values = map negate values

negamaxMoves :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> State -> Int -> (Int, [(Double, Move)])
negamaxMoves gameStatus evaluate move state depth
    = case negamaxChildNodes (gameStatus state) depth of
        Nothing    -> (1, [])
        Just moves -> (sum nodes, minimumsPair movevs)
                      where (nodes, movevs) = unzip [(\(n,v) -> (n,(v,m))) $ doNegamax (move m state) depth
                                                    | m <- moves]
                            doNegamax = negamax gameStatus evaluate move


negamaxMovesID :: (String -> IO ()) -> (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> NominalDiffTime -> State -> IO (Int, Int, [(Double, Move)])
negamaxMovesID debugLn gameStatus evaluate move seconds state = iterativelyDeepen debugLn (\s d -> return (negamaxMoves gameStatus evaluate move s d)) seconds state

