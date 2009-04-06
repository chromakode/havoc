module Havoc.MiniChess.Move where

import Data.Ix (inRange)
import Data.List (unfoldr, union)
import Data.Array (bounds)
import Havoc.State
import Havoc.MiniChess.Game

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq, Enum)
type Move = (Square, Square)

-- Take alternate items from a list of lists
-- E.g. [[1,2], [3,4]] => [1,3,2,4]
stripe :: [[a]] -> [a]
stripe =  concat . (unfoldr next)
       where next b = if (not (null b'))
                          then Just (map head b', map tail b')
                          else Nothing
                    where b' = filter (not . null) b

move :: Direction -> Square -> Square
move North     (i,j) = (i-1,j)
move South     (i,j) = (i+1,j)
move East      (i,j) = (i,j+1)
move West      (i,j) = (i,j-1)
move Northeast (i,j) = (i-1,j+1)
move Southeast (i,j) = (i+1,j+1)
move Southwest (i,j) = (i+1,j-1)
move Northwest (i,j) = (i-1,j-1)

moveLine :: Direction -> BoardSize -> Square -> [Square]
moveLine direction size square = takeWhile (inRange size) $ drop 1 (iterate (move direction) square)

moves :: [Direction] -> BoardSize -> Square -> [Square]
moves directions size square = filter (inRange size) $ map (`move` square) directions

moveLines :: [Direction] -> BoardSize -> Square -> [Square]
moveLines directions size square = stripe $ map (\d -> moveLine d size square) directions

knightMoves :: BoardSize -> Square -> [Square]
knightMoves size (i,j) = filter (inRange size) $ concat [[(i+da,j+db), (i+db,j+da)] | da <- [-2,2], db <- [-1,1]]

chessMoves :: BoardSize -> Position -> [Square]
chessMoves size (square, Piece _     King)   = moves [North .. Northwest] size square
chessMoves size (square, Piece _     Queen)  = moveLines [North .. Northwest] size square
chessMoves size (square, Piece _     Rook)   = moveLines [North, East, South, West] size square
chessMoves size (square, Piece _     Bishop) = (moves [North, East, South, West] size square) `union` (moveLines [Northeast, Southeast, Southwest, Northwest] size square)
chessMoves size (square, Piece _     Knight) = knightMoves size square
chessMoves size (square, Piece White Pawn)   = moves [North] size square
chessMoves size (square, Piece Black Pawn)   = moves [South] size square

moveGen :: State -> [Move]
moveGen state = stripe [map ((,) square) (chessMoves (bounds board') position)
                           | position@(square, Piece pieceColor _) <- pieces board'
                           , pieceColor == (color state) ]
              where board' = board state
