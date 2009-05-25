module Havoc.Game.MiniChess.Move where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game.State
import Havoc.Game.Move

mcMoves :: GameState s -> Position -> ST s [Square]
mcMoves state (square, Piece _     King)   = dirMoves MoveCapture [North .. Northwest] state square
mcMoves state (square, Piece _     Queen)  = lineMoves MoveCapture [North .. Northwest] state square
mcMoves state (square, Piece _     Rook)   = lineMoves MoveCapture [North, East, South, West] state square
mcMoves state (square, Piece _     Bishop) = (dirMoves Move [North, East, South, West] state square) +..+ (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
mcMoves state (square, Piece _     Knight) = knightMoves state square
mcMoves state (square, Piece White Pawn)   = (dirMoves Move [North] state square) +..+ (dirMoves Capture [Northwest, Northeast] state square)
mcMoves state (square, Piece Black Pawn)   = (dirMoves Move [South] state square) +..+ (dirMoves Capture [Southwest, Southeast] state square)
mcMoves state (square, Blank) = error "Move.mcMoves: moves for blank square requested"

handlePromotion :: Square -> GameState s -> ST s ()
handlePromotion toSquare state@(GameState turn turnColor board) = do
    piece <- readArray board toSquare
    let pieceColor = colorOf piece
        isPawn = (pieceType piece) == Pawn

    edge <- endRow pieceColor board
    let isEndRow = (fst toSquare) == edge

    if isPawn && isEndRow
        then writeArray board toSquare (Piece pieceColor Queen)
        else return ()

mcMove :: GameState s -> Move -> ST s (GameState s, MoveDiff)
mcMove state move@(fromSquare, toSquare) = do
    (newState, diff) <- chessDoMove state move
    handlePromotion toSquare newState
    return (newState, diff)
