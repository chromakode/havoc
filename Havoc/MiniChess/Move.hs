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

takeWhileAnd :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
takeWhileAnd p f xs = ys ++ f zs
    where (ys,zs)   = span p xs

takeIf :: Int -> (a -> Bool) -> [a] -> [a]
takeIf n p = (filter p) . (take n)

dirMove :: Direction -> Square -> Square
dirMove North     (i,j) = (i-1,j)
dirMove South     (i,j) = (i+1,j)
dirMove East      (i,j) = (i,j+1)
dirMove West      (i,j) = (i,j-1)
dirMove Northeast (i,j) = (i-1,j+1)
dirMove Southeast (i,j) = (i+1,j+1)
dirMove Southwest (i,j) = (i+1,j-1)
dirMove Northwest (i,j) = (i-1,j-1)

canCapture state = not . isTurnColor state

validPointMove :: MoveType -> State -> Square -> Bool
validPointMove capture state@(State turn turnColor board) square
    =  inRange (bounds board) square
    && case capture of
         Move        -> isBlank board square
         Capture     -> (not . isBlank board) square && canCapture state square
         MoveCapture -> isBlank board square || canCapture state square

validPointMoves :: MoveType -> State -> [Square] -> [Square]
validPointMoves capture state = filter (validPointMove capture state)

dirMoves :: MoveType -> [Direction] -> State -> Square -> [Square]
dirMoves capture directions state square
    = validPointMoves capture state
    $ map (`dirMove` square) directions

lineMove :: MoveType -> Direction -> State -> Square -> [Square]
lineMove capture direction state@(State turn turnColor board) square
    = untilBlocked
    . takeWhile (inRange (bounds board))
    . drop 1
    $ iterate (dirMove direction) square
    where
        untilBlocked
            = case capture of
                Move        -> takeWhile (isBlank board)
                Capture     -> takeCaptureMove . dropWhile (isBlank board)
                MoveCapture -> takeWhileAnd (isBlank board) takeCaptureMove
                
        takeCaptureMove = takeIf 1 (canCapture state)

lineMoves :: MoveType -> [Direction] -> State -> Square -> [Square]
lineMoves capture directions state square
    = stripe 
    $ map (\d -> lineMove capture d state square) directions

knightMoves :: State -> Square -> [Square]
knightMoves state square@(i,j)
    = validPointMoves MoveCapture state
    $ concat [[(i+da,j+db), (i+db,j+da)] | da <- [-2,2], db <- [-1,1]]

chessMoves :: State -> Position -> [Square]
chessMoves state (square, Piece _     King)   = dirMoves MoveCapture [North .. Northwest] state square
chessMoves state (square, Piece _     Queen)  = lineMoves MoveCapture [North .. Northwest] state square
chessMoves state (square, Piece _     Rook)   = lineMoves MoveCapture [North, East, South, West] state square
chessMoves state (square, Piece _     Bishop) = (dirMoves Move [North, East, South, West] state square) `union` (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
chessMoves state (square, Piece _     Knight) = knightMoves state square
chessMoves state (square, Piece White Pawn)   = (dirMoves Move [North] state square) `union` (dirMoves Capture [Northwest, Northeast] state square)
chessMoves state (square, Piece Black Pawn)   = (dirMoves Move [South] state square) `union` (dirMoves Capture [Southwest, Southeast] state square)

moveGenPosition :: State -> Position -> [Move]
moveGenPosition state position@(fromSquare, _) = map ((,) fromSquare) (chessMoves state position)

moveGenSquare :: State -> Square -> [Move]
moveGenSquare state fromSquare = moveGenPosition state position
    where position = (fromSquare, (board state) ! fromSquare)

moveGen :: State -> [Move]
moveGen state@(State turn turnColor board) =
    stripe [moveGenPosition state position
               | position@(square, Piece pieceColor _) <- positions board
               , pieceColor == turnColor ]
              
move :: Move -> State -> State
move (fromSquare, toSquare) (State turn turnColor board)
    | movedPiece == Blank     = error ("Move.move: no piece at position " ++ (show fromSquare))
    | turnColor /= movedColor = error "Move.move: piece does not belong to color on move"
    | otherwise               = State (turn+1) (invertColor turnColor) (board // [(fromSquare, Blank), (toSquare, movedPiece)])
    where
        movedPiece = board ! fromSquare
        movedColor = colorOf movedPiece

validMove :: State -> Move -> Bool
validMove state move@(fromSquare, _) = move `elem` (moveGenSquare state fromSquare)
