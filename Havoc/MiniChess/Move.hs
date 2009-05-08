module Havoc.MiniChess.Move where

import Data.List (union)
import Data.Array ((!), (//))
import Havoc.State
import Havoc.Move

miniChessMoves :: State -> Position -> [Square]
miniChessMoves state (square, Piece _     King)   = dirMoves MoveCapture [North .. Northwest] state square
miniChessMoves state (square, Piece _     Queen)  = lineMoves MoveCapture [North .. Northwest] state square
miniChessMoves state (square, Piece _     Rook)   = lineMoves MoveCapture [North, East, South, West] state square
miniChessMoves state (square, Piece _     Bishop) = (dirMoves Move [North, East, South, West] state square) `union` (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
miniChessMoves state (square, Piece _     Knight) = knightMoves state square
miniChessMoves state (square, Piece White Pawn)   = (dirMoves Move [North] state square) `union` (dirMoves Capture [Northwest, Northeast] state square)
miniChessMoves state (square, Piece Black Pawn)   = (dirMoves Move [South] state square) `union` (dirMoves Capture [Southwest, Southeast] state square)
miniChessMoves state (square, Blank) = error "Move.miniChessMoves: moves for blank square requested"

moveGen :: State -> [Move]
moveGen = genericMoveGen miniChessMoves 

promotePawn :: Square -> Board -> Board
promotePawn square board
    = if isPawn && isEndRow
        then board // [(square, Piece pieceColor Queen)]
        else board
    where
        piece = board ! square
        pieceColor = colorOf piece
        
        isPawn = (pieceType piece) == Pawn
        isEndRow = (fst square) == (endRow pieceColor board)

move :: Move -> State -> State
move squares@(fromSquare, toSquare) state
    = State turn turnColor (promotePawn toSquare board)
    where
        State turn turnColor board = genericMove squares state
