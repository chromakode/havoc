module Havoc.Game.MiniChess.Evaluate where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Player.DoUndo

winScore :: GameState s -> Score
winScore (GameState turn turnColor board pieceMap) = max_eval_score - (turn * 2)

mcEvaluateResult :: GameState s -> Result -> ST s Score
mcEvaluateResult state@(GameState turn turnColor board pieceMap) result
    = case result of
        Draw        -> return 0
        Win color   -> let sign = if color == turnColor then 1 else -1 in
                       return $ sign * (winScore state)

lastColorSign :: Color -> Int
lastColorSign color = case invertColor color of
                    White -> 1
                    Black -> -1

mcEvaluateMove :: Score -> GameState s -> MoveDiff -> ST s Score
mcEvaluateMove oldValue state diff@(MoveDiff movedPiece _ takenPiece _) = do
    let sign = lastColorSign (turnColor state)
    if takenPiece /= Blank && (pieceType takenPiece) == King
      then return $ sign * (winScore state)
      else do delta <- liftM sum $ mapM (\f -> f state diff) [ naiveMaterialScore ]
              return $ oldValue + (sign * delta)

naiveMaterialScore :: GameState s -> MoveDiff -> ST s Score
naiveMaterialScore (GameState turn turnColor board pieceMap) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) = do
    return $ (score becomePiece - score movedPiece) + score takenPiece
    where
        score Blank                  = 0        
        score (Piece _ color Pawn)   = 100
        score (Piece _ color Knight) = 300
        score (Piece _ color Bishop) = 500
        score (Piece _ color Rook)   = 500
        score (Piece _ color Queen)  = 900
        score (Piece _ color King)   = 0
