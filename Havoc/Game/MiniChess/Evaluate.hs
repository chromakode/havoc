module Havoc.Game.MiniChess.Evaluate where

import Havoc.State
import Havoc.Game
import Havoc.Game.MiniChess.Game

evaluate :: Status -> Double
evaluate status
    = case status of
        End state (Win color) -> if color == (turnColor state) then 10 else -10
        End _ Draw            -> 0.5
        Continue state _      -> (sum . map ($status)) [ naiveMaterialScore
                                                       , coverageScore      ]

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
