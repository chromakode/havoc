module Havoc.Player.DoUndo where

import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State

mapMoves :: (Game a) => a s -> (a s -> ST s b) -> [Move] -> ST s [b]
mapMoves state f [] = return []
mapMoves state f (m:moves) = do
    (state, undo) <- doMove state m
    result <- f state
    undoMove state undo
    tail <- mapMoves state f moves
    return $ result : tail
