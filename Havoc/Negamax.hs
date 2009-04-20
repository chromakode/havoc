module Havoc.Negamax where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Clock
import System.Timeout
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
        
negamaxMovesID :: (State -> Status) -> (State -> Double) -> (Move -> State -> State) -> NominalDiffTime -> State -> IO [Move]
negamaxMovesID gameStatus evaluate move seconds state
    = do startTime <- getCurrentTime
         run startTime 0 []
         
    where
        runDepth depth = return $! negamaxMoves gameStatus evaluate move depth state
        
        run startTime curDepth lastResult
              = do curTime <- getCurrentTime
                   let remain = seconds - (diffUTCTime curTime startTime)
                   let remainMicroseconds = floor (remain * 10^6)
                   if remainMicroseconds < 0
                       then return lastResult
                       else do result <- timeout remainMicroseconds (runDepth curDepth)
                               case result of
                                 Nothing     -> return lastResult
                                 Just result -> run startTime (curDepth+1) result

