module Havoc.Game.MiniChess.Evaluate where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Player.DoUndo

mcEvaluateMove :: EvaluatedState s -> GameStatus -> MoveDiff -> ST s Int
mcEvaluateMove state status diff
    = case status of
        End (Win color) -> return $ gameOverScore color state
        End Draw        -> return 0
        Continue _      -> (liftM sum) . mapM (\f -> f state diff)) [ naiveMaterialScore ]

gameOverScore :: Color -> GameState s -> Int
gameOverScore winColor state
    = sign * max_eval_score
    + (-sign) * turnNum * 2
    where 
        sign = if winColor == (turnColor state) then 1 else -1
        turnNum = turn state

naiveMaterialScore :: GameState s -> MoveDiff -> ST s Int
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
        typeScore King   = 0
