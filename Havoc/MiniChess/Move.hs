module Havoc.MiniChess.Move where

import Data.List (union)
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

moveGen = genericMoveGen miniChessMoves 
