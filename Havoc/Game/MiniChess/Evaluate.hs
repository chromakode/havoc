module Havoc.Game.MiniChess.Evaluate where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Player.DoUndo

mcEvaluateResult :: GameState s -> Result -> ST s Score
mcEvaluateResult (GameState turn turnColor board) result
    = case result of
        Draw        -> return 0
        Win color   -> let sign = if color == turnColor then 1 else -1 in
                       return $ sign * max_eval_score
                              + (-sign) * turn * 2 

lastColorSign :: Color -> Int
lastColorSign color = case invertColor color of
                    White -> 1
                    Black -> -1

mcEvaluateMove :: Score -> GameState s -> MoveDiff -> ST s Score
mcEvaluateMove oldValue state diff = do
    delta <- liftM sum $ mapM (\f -> f state diff) [ naiveMaterialScore ]
    let sign = lastColorSign (turnColor state)
    return $ oldValue + (sign * delta)

naiveMaterialScore :: GameState s -> MoveDiff -> ST s Score
naiveMaterialScore (GameState turn turnColor board) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) = do
    return $ (score becomePiece - score movedPiece) + score takenPiece
    where
        score Blank = 0
        score (Piece color pieceType) = (colorScore color) * (typeScore pieceType)
        
        colorScore color = if color == turnColor then 1 else -1
        
        typeScore Pawn   = 100
        typeScore Knight = 300
        typeScore Bishop = 500
        typeScore Rook   = 500
        typeScore Queen  = 900
        typeScore King   = 9000
