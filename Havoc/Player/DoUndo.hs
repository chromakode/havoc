module Havoc.Player.DoUndo where

import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State

mapMoves :: (Game a) => a s -> (Move -> a s -> ST s b) -> [Move] -> ST s [b]
mapMoves state f [] = return []
mapMoves state f (m:moves) = do
    result <- doUndo state m f
    tail <- mapMoves state f moves
    return $ result : tail
    
doUndo :: (Game a) => a s -> Move -> (Move -> a s -> ST s b) -> ST s b
doUndo state m f = do
    (state, diff) <- doMove state m
    result <- f m state
    state <- undoMove state diff
    return result
    
doUndoIO :: (Game a) => a RealWorld -> Move -> (Move -> a RealWorld -> IO b) -> IO b
doUndoIO state m f = do
    (state, diff) <- stToIO $ doMove state m
    result <- f m state
    state <- stToIO $ undoMove state diff
    return result


