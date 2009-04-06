module Havoc.MiniChess.Move where

import Data.List (unfoldr, union)
import Havoc.State
import Havoc.MiniChess.Game

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq, Enum)
type Move = (Square, Square)

-- Take alternate items from a list of lists
-- E.g. [[1,2], [3,4]] => [1,3,2,4]
stripe :: [[a]] -> [a]
stripe =  concat . (unfoldr next)
       where next b = if (not (all null b))
                          then Just (map head b, filter (not . null) (map tail b))
                          else Nothing

move :: Direction -> Square -> Square
move North     (i,j) = (i+1,j)
move South     (i,j) = (i-1,j)
move East      (i,j) = (i,j+1)
move West      (i,j) = (i,j-1)
move Northeast (i,j) = (i+1,j+1)
move Southeast (i,j) = (i-1,j+1)
move Southwest (i,j) = (i-1,j-1)
move Northwest (i,j) = (i+1,j-1)

moves :: [Direction] -> Square -> [Square]
moves directions square = map (`move` square) directions

moveLine :: Direction -> Square -> [Square]
moveLine direction square = drop 1 (iterate (move direction) square)

moveLines :: [Direction] -> Square -> [Square]
moveLines directions square = stripe $ map (`moveLine` square) directions

knightMoves :: Square -> [Square]
knightMoves (i,j) = concat [[(i+da,j+db), (i+db,j+da)] | da <- [-2,2], db <- [-1,1]]

chessMoves :: Position -> [Square]
chessMoves (square, Piece _     King)   = moves [North .. Northwest] square
chessMoves (square, Piece _     Queen)  = moveLines [North .. Northwest] square
chessMoves (square, Piece _     Rook)   = moveLines [North, East, South, West] square
chessMoves (square, Piece _     Bishop) = (moves [North, East, South, West] square) `union` (moveLines [Northeast, Southeast, Southwest, Northwest] square)
chessMoves (square, Piece _     Knight) = knightMoves square
chessMoves (square, Piece White Pawn)   = [move North square]
chessMoves (square, Piece Black Pawn)   = [move South square]

moveGen :: State -> [Move]
moveGen state = stripe [map ((,) square) (chessMoves position) | position@(square, _) <- pieces (board state)]
