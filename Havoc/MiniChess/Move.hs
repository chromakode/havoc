module Havoc.MiniChess.Move where

import Data.Ix (inRange)
import Data.List (unfoldr, union)
import Data.Array ((!), (//), bounds)
import Havoc.State

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq, Enum)
type Move = (Square, Square)
data MoveType = Move | Capture | MoveCapture

-- Take alternate items from a list of lists
-- E.g. [[1,2], [3,4]] => [1,3,2,4]
stripe :: [[a]] -> [a]
stripe =  concat . (unfoldr next)
       where next b = if (not (null b'))
                          then Just (map head b', map tail b')
                          else Nothing
                    where b' = filter (not . null) b

-- From the HUGS prelude, modded
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x:xs) = x : if p x then takeWhile1 p xs else []
takeWhile1 p []     = []

dirMove :: Direction -> Square -> Square
dirMove North     (i,j) = (i-1,j)
dirMove South     (i,j) = (i+1,j)
dirMove East      (i,j) = (i,j+1)
dirMove West      (i,j) = (i,j-1)
dirMove Northeast (i,j) = (i-1,j+1)
dirMove Southeast (i,j) = (i+1,j+1)
dirMove Southwest (i,j) = (i+1,j-1)
dirMove Northwest (i,j) = (i-1,j-1)

validPointMove :: MoveType -> Board -> Square -> Bool
validPointMove capture board square
    =  inRange (bounds board) square
    && case capture of
         Move        -> isBlank board square
         Capture     -> (not . isBlank board) square
         MoveCapture -> True

validPointMoves :: MoveType -> Board -> [Square] -> [Square]
validPointMoves capture board = filter (validPointMove capture board)

dirMoves :: MoveType -> [Direction] -> Board -> Square -> [Square]
dirMoves capture directions board square
    = validPointMoves capture board
    $ map (`dirMove` square) directions

lineMove :: MoveType -> Direction -> Board -> Square -> [Square]
lineMove capture direction board square
    = untilBlocked
    . takeWhile (inRange (bounds board))
    . drop 1
    $ iterate (dirMove direction) square
    where
        untilBlocked
            = case capture of
                Move        -> takeWhile (isBlank board)
                Capture     -> take 1 . dropWhile (isBlank board)
                MoveCapture -> takeWhile1 (isBlank board)

lineMoves :: MoveType -> [Direction] -> Board -> Square -> [Square]
lineMoves capture directions board square
    = stripe 
    $ map (\d -> lineMove capture d board square) directions

knightMoves :: Board -> Square -> [Square]
knightMoves board square@(i,j)
    = validPointMoves MoveCapture board
    $ concat [[(i+da,j+db), (i+db,j+da)] | da <- [-2,2], db <- [-1,1]]

chessMoves :: Board -> Position -> [Square]
chessMoves board (square, Piece _     King)   = dirMoves MoveCapture [North .. Northwest] board square
chessMoves board (square, Piece _     Queen)  = lineMoves MoveCapture [North .. Northwest] board square
chessMoves board (square, Piece _     Rook)   = lineMoves MoveCapture [North, East, South, West] board square
chessMoves board (square, Piece _     Bishop) = (dirMoves Move [North, East, South, West] board square) `union` (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] board square)
chessMoves board (square, Piece _     Knight) = knightMoves board square
chessMoves board (square, Piece White Pawn)   = (dirMoves Move [North] board square) `union` (dirMoves Capture [Northwest, Northeast] board square)
chessMoves board (square, Piece Black Pawn)   = (dirMoves Move [South] board square) `union` (dirMoves Capture [Southwest, Southeast] board square)

moveGen :: State -> [Move]
moveGen state = stripe [map ((,) square) (chessMoves board' position)
                           | position@(square, Piece pieceColor _) <- pieces board'
                           , pieceColor == (color state) ]
              where board' = board state
              
move :: Move -> State -> State
move (fromSquare, toSquare) (State turn color board)
    | movedPiece == Blank = error ("Move.move: no piece at position " ++ (show fromSquare))
    | color /= movedColor = error "Move.move: piece does not belong to color on move"
    | otherwise           = State (turn+1) (invertColor color) (board // [(fromSquare, Blank), (toSquare, movedPiece)])
    where
        movedPiece = board ! fromSquare
        movedColor = colorOf movedPiece
        
