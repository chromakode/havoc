{-# LANGUAGE BangPatterns #-}

module Havoc.Player.IterativeDeepening where

import Data.Time.Clock
import System.Timeout
import Havoc.Game
import Havoc.Move
import Havoc.State

iterativelyDeepenC :: (State -> Int -> a -> (Int, [Move], a)) -> a -> NominalDiffTime -> State -> IO (Int, Int, [Move])
iterativelyDeepenC doSearch startData seconds state
    = do startTime <- getCurrentTime
         run startTime (1, 0, []) startData
         
    where
        runDepth depth continueData = return (nodes, moves, continueData')
                 where !(!nodes, !moves, !continueData') = doSearch state depth continueData 
        
        run startTime out@(curDepth, nodes, lastMoves) continueData
            = do curTime <- getCurrentTime
                 let elapsed = diffUTCTime curTime startTime
                 let remain = seconds - elapsed
                 let remainMicroseconds = floor (remain * 10^6)
                 if remainMicroseconds < 0
                     then return out
                     else do result <- timeout remainMicroseconds (runDepth curDepth continueData)
                             case result of
                               Nothing                            -> return out
                               Just (nodes', moves, continueData) -> run startTime ((curDepth+1), nodes+nodes', moves) continueData

iterativelyDeepen :: (State -> Int -> (Int, [Move])) -> NominalDiffTime -> State -> IO (Int, Int, [Move])
iterativelyDeepen doSearch
    = iterativelyDeepenC doSearchC ()
    where
        doSearchC state depth _
            = (nodes, moves, ())
            where (nodes, moves) = doSearch state depth
