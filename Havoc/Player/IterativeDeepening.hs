{-# LANGUAGE BangPatterns #-}

module Havoc.Player.IterativeDeepening where

import Control.Monad.ST
import Data.Time.Clock
import Numeric
import System.Timeout
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Notation
import Havoc.Player
import Havoc.Utils

iterativelyDeepen :: (Game a) => (String -> IO ()) -> (a RealWorld -> Int -> IO (Int, [(Int, Move)])) -> NominalDiffTime -> a RealWorld -> IO (Int, Int, [(Int, Move)])
iterativelyDeepen debugLn doSearch seconds state
    = do startTime <- getCurrentTime
         run startTime (0, 0, [])
    where
        runDepth depth = do
            state' <- stToIO $ copyState state
            !(!nodes, !moves) <- doSearch state' depth
            return (nodes, moves)
        
        run startTime out@(lastDepth, nodes, lastMoves)
            = do elapsed <- getElapsed startTime
                 let remain = seconds - elapsed
                 let remainMicroseconds = floor (remain * 10^6)
                 let tryDepth = if even lastDepth && lastDepth > 0 && lastDepth < 6
                                  then lastDepth+2 -- Skip to avoid horizon effect on low odd depths
                                  else lastDepth+1
                 if remainMicroseconds < 0
                     then return out
                     else do result <- timeout remainMicroseconds (runDepth tryDepth)
                             case result of
                               Nothing -> do
                                   debugLn $ "Depth " ++ show tryDepth ++ ": interrupted"
                                   debugLn $ "Searched " ++ show nodes ++ " nodes in " ++ show elapsed ++ " seconds (" ++ showSeconds 4 ((fromIntegral nodes) / elapsed) ++ " nodes/second)"
                                   return out
                               Just (nodes', moves) -> do
                                   scoredMoves <- stToIO $ showScoredMoves state moves
                                   elapsed <- getElapsed startTime
                                   debugLn $ "Depth " ++ show tryDepth ++ " (" ++ showSeconds 2 elapsed ++ "s) : " ++ scoredMoves
                                   run startTime (tryDepth, nodes+nodes', moves)
            where getElapsed startTime = do
                      curTime <- getCurrentTime
                      return $ diffUTCTime curTime startTime
