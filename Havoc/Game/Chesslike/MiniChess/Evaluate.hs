module Havoc.Game.Chesslike.MiniChess.Evaluate where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Chesslike.State

mcEvaluate :: GameState s -> GameStatus -> ST s Int
mcEvaluate state status
    = case status of
        End (Win color) -> return $ gameOverScore color state
        End Draw        -> return 0
        Continue _      -> ((liftM sum) . mapM ($state)) [ naiveMaterialScore ]

gameOverScore :: Color -> GameState s -> Int
gameOverScore winColor state
    = sign * max_eval_score
    + (-sign) * turnNum * 2
    where 
        sign = if winColor == (turnColor state) then 1 else -1
        turnNum = turn state

naiveMaterialScore :: GameState s -> ST s Int
naiveMaterialScore (GameState turn turnColor board) = do
    ps <- pieces board
    return $ (sum . (map score)) ps
    where
        score (Piece color pieceType) = (colorScore color) * (typeScore pieceType)
        
        colorScore color = if color == turnColor then 1 else -1
        
        typeScore Pawn   = 100
        typeScore Knight = 300
        typeScore Bishop = 500
        typeScore Rook   = 500
        typeScore Queen  = 900
        typeScore King   = 0
