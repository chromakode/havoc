{-# LANGUAGE BangPatterns #-}

module Havoc.Player.IterativeDeepening where

import Data.Time.Clock
import System.Timeout
import Havoc.Game
import Havoc.Move
import Havoc.Notation
import Havoc.Player
import Havoc.State

iterativelyDeepenC :: (String -> IO ()) -> (State -> Int -> a -> IO (Int, [(Double, Move)], a)) -> a -> NominalDiffTime -> State -> IO (Int, Int, [(Double, Move)])
iterativelyDeepenC debugLn doSearch startData seconds state
    = do startTime <- getCurrentTime
         run startTime (1, 0, []) startData
         
    where
        runDepth depth continueData = do
            !(!nodes, !moves, !continueData') <- doSearch state depth continueData
            return (nodes, moves, continueData')
        
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
                               Just (nodes', moves, continueData) -> do
                                   debugLn $ "Depth " ++ show curDepth ++ ": " ++ showScoredMoves state moves
                                   run startTime ((curDepth+1), nodes+nodes', moves) continueData

iterativelyDeepen :: (String -> IO ()) -> (State -> Int -> IO (Int, [(Double, Move)])) -> NominalDiffTime -> State -> IO (Int, Int, [(Double, Move)])
iterativelyDeepen debugLn doSearch
    = iterativelyDeepenC debugLn doSearchC ()
    where
        doSearchC state depth _ = do
            (nodes, moves) <- doSearch state depth
            return (nodes, moves, ())
