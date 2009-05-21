module Havoc.Game.MiniChess.Evaluate where

import Havoc.State
import Havoc.Game
import Havoc.Game.MiniChess.Game

evaluate :: Status -> Int
evaluate status
    = case status of
        End state (Win color) -> gameOverScore color state
        End _ Draw            -> 9950
        Continue state _      -> (sum . map ($status)) [ naiveMaterialScore ]

gameOverScore :: Color -> State -> Int
gameOverScore winColor state
    = sign * max_eval_score
    + (-sign) * turnNum * 2
    where 
        sign = if winColor == (turnColor state) then 1 else -1
        turnNum = turn state

naiveMaterialScore :: Status -> Int
naiveMaterialScore (Continue (State turn turnColor board) _)
    = (sum . (map score)) (pieces board)
    where
        score (Piece color pieceType) = (colorScore color) * (typeScore pieceType)
        
        colorScore color = if color == turnColor then 1 else -1
        
        typeScore Pawn   = 100
        typeScore Knight = 300
        typeScore Bishop = 500
        typeScore Rook   = 500
        typeScore Queen  = 900
        typeScore King   = 0

coverageScore :: Status -> Int
coverageScore status@(Continue _ moves)
    = (length moves)
