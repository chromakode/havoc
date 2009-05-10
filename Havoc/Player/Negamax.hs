{-# LANGUAGE BangPatterns #-}

module Havoc.Player.Negamax where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Clock
import System.Timeout
import Havoc.Game
import Havoc.Move
import Havoc.State
import Havoc.Utils

negamax :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> Int -> State -> (Int, Double)
negamax gameStatus evaluate move depth state
    | depth == 0  = heuristicValue
    | otherwise   = case status of
                      End _ _          -> heuristicValue
                      Continue _ moves -> negamaxValue moves
    where
        status = gameStatus state
        heuristicValue = (1, evaluate status)
        
        recurse = negamax gameStatus evaluate move
        
        negamaxValue moves = (sum nodes, maximum negvalues)
                           where (nodes, negvalues) = unzip [recurse (depth-1) (move m state)
                                                            | m <- moves]
                                 values = map negate values

negamaxMoves :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> Int -> State -> (Int, [Move])
negamaxMoves gameStatus evaluate move depth state
    = case gameStatus state of
        End _ _          -> (1, [])
        Continue _ moves -> (sum nodes, minimumsPair movevs)
                            where (nodes, movevs) = unzip [(\(n,v) -> (n,(v,m))) $ doNegamax (move m state)
                                                          | m <- moves]
    where
        doNegamax = negamax gameStatus evaluate move depth
        
negamaxMovesID :: (State -> Status) -> (Status -> Double) -> (Move -> State -> State) -> NominalDiffTime -> State -> IO (Int, Int, [Move])
negamaxMovesID gameStatus evaluate move seconds state
    = do startTime <- getCurrentTime
         run startTime (0, 0, [])
         
    where
        runDepth depth = return (nodes, moves)
                       where !(!nodes, !moves) = negamaxMoves gameStatus evaluate move depth state
        
        run startTime out@(curDepth, nodes, lastMoves)
              = do curTime <- getCurrentTime
                   let remain = seconds - (diffUTCTime curTime startTime)
                   let remainMicroseconds = floor (remain * 10^6)
                   if remainMicroseconds < 0
                       then return out
                       else do result <- timeout remainMicroseconds (runDepth curDepth)
                               case result of
                                 Nothing              -> return out
                                 Just (nodes', moves) -> run startTime ((curDepth+1), nodes+nodes', moves)

