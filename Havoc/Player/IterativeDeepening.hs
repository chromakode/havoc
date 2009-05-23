{-# LANGUAGE BangPatterns #-}

module Havoc.Player.IterativeDeepening where

import Data.Time.Clock
import Numeric
import System.Timeout
import Havoc.Game
import Havoc.Move
import Havoc.Notation
import Havoc.Player
import Havoc.State
import Havoc.Utils

iterativelyDeepenC :: (String -> IO ()) -> (State -> Int -> a -> IO (Int, [(Int, Move)], a)) -> a -> NominalDiffTime -> State -> IO (Int, Int, [(Int, Move)])
iterativelyDeepenC debugLn doSearch startData seconds state
    = do startTime <- getCurrentTime
         run startTime (0, 0, []) startData
         
    where
        runDepth depth continueData = do
            !(!nodes, !moves, !continueData') <- doSearch state depth continueData
            return (nodes, moves, continueData')
        
        run startTime out@(lastDepth, nodes, lastMoves) continueData
            = do curTime <- getCurrentTime
                 let elapsed = diffUTCTime curTime startTime
                 let remain = seconds - elapsed
                 let remainMicroseconds = floor (remain * 10^6)
                 let tryDepth = if even lastDepth && lastDepth > 0 && lastDepth < 6
                                  then lastDepth+2 -- Skip to avoid horizon effect on low odd depths
                                  else lastDepth+1
                 if remainMicroseconds < 0
                     then return out
                     else do result <- timeout remainMicroseconds (runDepth tryDepth continueData)
                             case result of
                               Nothing                            -> do
                                   debugLn $ "Depth " ++ show tryDepth ++ ": interrupted"
                                   debugLn $ "Searched " ++ show nodes ++ " nodes in " ++ show elapsed ++ " seconds (" ++ printSeconds 4 ((fromIntegral nodes) / elapsed) ++ " nodes/second)"
                                   return out
                               Just (nodes', moves, continueData) -> do
                                   debugLn $ "Depth " ++ show tryDepth ++ ": " ++ showScoredMoves state moves
                                   run startTime (tryDepth, nodes+nodes', moves) continueData

iterativelyDeepen :: (String -> IO ()) -> (State -> Int -> IO (Int, [(Int, Move)])) -> NominalDiffTime -> State -> IO (Int, Int, [(Int, Move)])
iterativelyDeepen debugLn doSearch
    = iterativelyDeepenC debugLn doSearchC ()
    where
        doSearchC state depth _ = do
            (nodes, moves) <- doSearch state depth
            return (nodes, moves, ())
