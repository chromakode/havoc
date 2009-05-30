module Havoc.Game.MiniChess.Move where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Game.MiniChess.Evaluate

mcMoves :: GameState s -> Position -> ST s [Square]
mcMoves state (square, Piece _     King)   = dirMoves MoveCapture [North .. Northwest] state square
mcMoves state (square, Piece _     Queen)  = lineMoves MoveCapture [North .. Northwest] state square
mcMoves state (square, Piece _     Rook)   = lineMoves MoveCapture [North, East, South, West] state square
mcMoves state (square, Piece _     Bishop) = (dirMoves Move [North, East, South, West] state square) +..+ (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
mcMoves state (square, Piece _     Knight) = knightMoves state square
mcMoves state (square, Piece White Pawn)   = (dirMoves Move [North] state square) +..+ (dirMoves Capture [Northwest, Northeast] state square)
mcMoves state (square, Piece Black Pawn)   = (dirMoves Move [South] state square) +..+ (dirMoves Capture [Southwest, Southeast] state square)
mcMoves state (square, Blank) = error "Move.mcMoves: moves for blank square requested"

handlePromotion :: GameState s -> MoveDiff -> ST s (MoveDiff)
handlePromotion (GameState turn turnColor board) diff@(MoveDiff movedPiece (fromSquare, toSquare) takenPiece _) = do
    let pieceColor = colorOf movedPiece
        isPawn = (pieceType movedPiece) == Pawn

    edge <- endRow pieceColor board
    let isEndRow = (fst toSquare) == edge

    if isPawn && isEndRow
        then do let becomePiece = (Piece pieceColor Queen)
                writeArray board toSquare becomePiece
                return $ MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece
        else return diff

mcMove :: Evaluated (GameState s) -> Move -> ST s (Evaluated (GameState s), Evaluated MoveDiff)
mcMove es@(Evaluated oldValue state) move@(fromSquare, toSquare) = do
    (newState, diff) <- chessDoMove state move
    diff <- handlePromotion newState diff
    newValue <- mcEvaluateMove oldValue newState diff
    return (Evaluated newValue newState, Evaluated oldValue diff)
