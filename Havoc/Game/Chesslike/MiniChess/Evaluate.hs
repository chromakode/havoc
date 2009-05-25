module Havoc.Game.Chesslike.MiniChess.Evaluate where

import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Chesslike.State
import Havoc.Game.Chesslike.MiniChess.Game

mcEvaluate :: MCState s -> Status -> Int
mcEvaluate mcState status
    = case status of
        End (Win color) -> gameOverScore color state
        End Draw        -> 0
        Continue _      -> (sum . map ($state)) [ naiveMaterialScore ]

gameOverScore :: Color -> MCState s -> ST s Int
gameOverScore winColor mcState@(MCState state)
    = sign * max_eval_score
    + (-sign) * turnNum * 2
    where 
        sign = if winColor == (turnColor state) then 1 else -1
        turnNum = turn state

naiveMaterialScore :: MCState s -> ST s Int
naiveMaterialScore (MCState (GameState turn turnColor board)) = do
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
