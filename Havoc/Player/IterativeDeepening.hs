{-# LANGUAGE BangPatterns #-}

module Havoc.Player.IterativeDeepening where

import Data.Time.Clock
import Numeric
import System.Timeout
import Havoc.Game
import Havoc.Notation
import Havoc.Player
import Havoc.Utils

iterativelyDeepen :: (String -> IO ()) -> (a s -> Int -> IO (Int, [(Int, Move)])) -> NominalDiffTime -> a s -> IO (Int, Int, [(Int, Move)])
iterativelyDeepen debugLn doSearch seconds state
    = do startTime <- getCurrentTime
         run startTime (0, 0, [])
    where
        runDepth depth = stToIO $ do
            state' <- copyState state
            !(!nodes, !moves) <- doSearch state' depth
            return (nodes, moves)
        
        run startTime out@(lastDepth, nodes, lastMoves)
            = do curTime <- getCurrentTime
                 let elapsed = diffUTCTime curTime startTime
                 let remain = seconds - elapsed
                 let remainMicroseconds = floor (remain * 10^6)
                 let tryDepth = if even lastDepth && lastDepth > 0 && lastDepth < 6
                                  then lastDepth+2 -- Skip to avoid horizon effect on low odd depths
                                  else lastDepth+1
                 if remainMicroseconds < 0
                     then return out
                     else do result <- timeout remainMicroseconds (runDepth tryDepth)
                             case result of
                               Nothing                            -> do
                                   debugLn $ "Depth " ++ show tryDepth ++ ": interrupted"
                                   debugLn $ "Searched " ++ show nodes ++ " nodes in " ++ show elapsed ++ " seconds (" ++ printSeconds 4 ((fromIntegral nodes) / elapsed) ++ " nodes/second)"
                                   return out
                               Just (nodes', moves) -> do
                                   debugLn $ "Depth " ++ show tryDepth ++ ": " ++ showScoredMoves state moves
                                   run startTime (tryDepth, nodes+nodes', moves)
