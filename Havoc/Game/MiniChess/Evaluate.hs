module Havoc.Game.MiniChess.Evaluate where

import Havoc.State
import Havoc.Game
import Havoc.Game.MiniChess.Game

evaluate :: Status -> Double
evaluate status
    = case status of
        End state (Win color) -> gameOverScore color state
        End _ Draw            -> 0.95
        Continue state _      -> (sum . map ($status)) [ (0.95*) . naiveMaterialScore ]

gameOverScore :: Color -> State -> Double
gameOverScore winColor state
    = sign
    + 0.04 * (-sign) * (turnNum/40)
    where 
        sign = if winColor == (turnColor state) then 1 else -1
        turnNum = (fromIntegral . turn) state

naiveMaterialScore :: Status -> Double
naiveMaterialScore (Continue (State turn turnColor board) _)
    = ((sum . (map score)) (pieces board)) / maxScore
    where
        score (Piece color pieceType) = (colorScore color) * (typeScore pieceType)
        
        colorScore color = if color == turnColor then 1 else -1
        
        typeScore Pawn   = 1
        typeScore Knight = 3
        typeScore Bishop = 5
        typeScore Rook   = 5
        typeScore Queen  = 9
        typeScore King   = 0

        maxScore = 5*(typeScore Pawn)
                 + (typeScore Knight)
                 + (typeScore Bishop)
                 + (typeScore Rook)
                 + (typeScore Queen)

coverageScore :: Status -> Double
coverageScore status@(Continue _ moves)
    = (fromIntegral (length moves)) / 100
