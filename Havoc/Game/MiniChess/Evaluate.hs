module Havoc.Game.MiniChess.Evaluate where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Player.DoUndo

winScore :: GameState s -> Score
winScore (GameState turn turnColor board) = max_eval_score - (turn * 2)

mcEvaluateResult :: GameState s -> Result -> ST s Score
mcEvaluateResult state@(GameState turn turnColor board) result
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
    if takenPiece /= '.' && (pieceType takenPiece) == 'K'
      then return $ sign * (winScore state)
      else do delta <- liftM sum $ mapM (\f -> f state diff) [ naiveMaterialScore ]
              return $ oldValue + (sign * delta)

naiveMaterialScore :: GameState s -> MoveDiff -> ST s Score
naiveMaterialScore (GameState turn turnColor board) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) = do
    return $ (typeScore becomePiece - typeScore movedPiece) + typeScore takenPiece
    where
        typeScore = score . pieceType
        score '.' = 0        
        score 'P' = 100
        score 'N' = 300
        score 'B' = 500
        score 'R' = 500
        score 'Q' = 900
        score 'K' = 0
