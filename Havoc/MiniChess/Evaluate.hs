module Havoc.MiniChess.Evaluate where

import Havoc.State
import Havoc.Game
import Havoc.MiniChess.Game

evaluate :: State -> Double
evaluate state@(State turn turnColor board)
    = case (gameStatus state) of
        End (Win color) -> if color == turnColor then 1 else -1
        End Draw        -> 0.5
        Continue _ _    -> naiveMaterialScore state

naiveMaterialScore :: State -> Double
naiveMaterialScore (State turn turnColor board)
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
