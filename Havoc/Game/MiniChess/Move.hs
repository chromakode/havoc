module Havoc.Game.MiniChess.Move where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Game.MiniChess.Evaluate

mcMoves :: GameState s -> Position -> ST s [Square]
mcMoves state (square, piece) = moves piece
    where
        moves 'K' = kingMoves
        moves 'k' = kingMoves
        moves 'Q' = queenMoves
        moves 'q' = queenMoves
        moves 'R' = rookMoves
        moves 'r' = rookMoves
        moves 'B' = bishopMoves
        moves 'b' = bishopMoves
        moves 'N' = knightMoves'
        moves 'n' = knightMoves'
        moves 'P' = (dirMoves Move [North] state square) +..+ (dirMoves Capture [Northwest, Northeast] state square)
        moves 'p' = (dirMoves Move [South] state square) +..+ (dirMoves Capture [Southwest, Southeast] state square)
        moves '.' = error "Move.mcMoves: moves for blank square requested"

        kingMoves = dirMoves MoveCapture [North .. Northwest] state square
        queenMoves = lineMoves MoveCapture [North .. Northwest] state square
        rookMoves = lineMoves MoveCapture [North, East, South, West] state square
        bishopMoves = (dirMoves Move [North, East, South, West] state square) +..+ (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
        knightMoves' = knightMoves state square

handlePromotion :: GameState s -> MoveDiff -> ST s (MoveDiff)
handlePromotion (GameState turn turnColor board) diff@(MoveDiff movedPiece (fromSquare, toSquare) takenPiece _) = do
    let pieceColor = colorOf movedPiece
        isPawn = (pieceType movedPiece) == 'P'

    edge <- endRow pieceColor board
    let isEndRow = (fst toSquare) == edge

    if isPawn && isEndRow
        then do let becomePiece = toColor 'Q' (colorOf movedPiece)
                writeArray board toSquare becomePiece
                return $ MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece
        else return diff

mcMove :: Evaluated (GameState s) -> Move -> ST s (Evaluated (GameState s), Evaluated MoveDiff)
mcMove es@(Evaluated oldValue state) move@(fromSquare, toSquare) = do
    (newState, diff) <- chessDoMove state move
    diff <- handlePromotion newState diff
    newValue <- mcEvaluateMove oldValue newState diff
    return (Evaluated newValue newState, Evaluated oldValue diff)
