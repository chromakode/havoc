module Havoc.Move where

import Control.Monad
import Control.Monad.ST
import Data.Ix (inRange)
import Data.Array.ST
import Havoc.State

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq, Enum)
type Move = (Square, Square)
data MoveType = Move | Capture | MoveCapture
type PieceMoveGen = GameState -> Position -> [Square]

dirMove :: Direction -> Square -> Square
dirMove North     (i,j) = (i-1,j)
dirMove South     (i,j) = (i+1,j)
dirMove East      (i,j) = (i,j+1)
dirMove West      (i,j) = (i,j-1)
dirMove Northeast (i,j) = (i-1,j+1)
dirMove Southeast (i,j) = (i+1,j+1)
dirMove Southwest (i,j) = (i+1,j-1)
dirMove Northwest (i,j) = (i-1,j-1)

canCapture :: GameState s -> ST s Bool
canCapture state = not . isTurnColor state

validPointMove :: MoveType -> GameState s -> Square -> ST s Bool
validPointMove capture state@(GameState turn turnColor board) square = do
    bounds <- getBounds board
    inRange bounds square
      && case capture of
           Move        -> isBlank board square
           Capture     -> all $ sequence [ (not . isBlank board) square
                                         , canCapture state square]
           MoveCapture -> any $ sequence [ isBlank board square
                                         , canCapture state square]

validPointMoves :: MoveType -> GameState s -> [Square] -> ST s [Square]
validPointMoves capture state = filterM (validPointMove capture state)

dirMoves :: MoveType -> [Direction] -> GameState s -> Square -> ST s [Square]
dirMoves capture directions state square
    = validPointMoves capture state
    $ map (`dirMove` square) directions

lineMove :: MoveType -> Direction -> GameStates  -> Square -> ST s [Square]
lineMove capture direction state@(GameState turn turnColor board) square = do
    bounds <- getBounds board
    untilBlocked
        . takeWhile (inRange bounds)
        . drop 1
        $ iterate (dirMove direction) square
    where
        untilBlocked [] = return []
        untilBlocked (s:squares) = case capture of
            Move -> do
                blank <- isBlank board s
                if blank
                    then continue $ Just s
                    else return []
                    
            Capture -> do
                blank   <- isBlank board s
                capture <- canCapture state s
                if blank
                    then continue Nothing
                    else doCapture capture s
            
            MoveCapture -> do
                blank   <- isBlank board s
                capture <- canCapture state s
                if blank
                    then continue $ Just s
                    else doCapture capture s
            
            where
                continue (Just s) = untilBlocked squares >>= (\tail -> s : tail)
                continue Nothing  = untilBlocked squares
                doCapture capture s = if capture then [s] else []

lineMoves :: MoveType -> [Direction] -> GameState s -> Square -> ST s [Square]
lineMoves capture directions state square
    = (concat . mapM) (\d -> lineMove capture d state square) directions

knightMoves :: GameState s -> Square -> ST s [Square]
knightMoves state square@(i,j)
    = validPointMoves MoveCapture state
    $ concat [[(i+da,j+db), (i+db,j+da)] | da <- [-2,2], db <- [-1,1]]

moveGenPosition :: PieceMoveGen -> GameState s -> Position -> ST s [Move]
moveGenPosition pieceMoves state position@(fromSquare, _) = map ((,) fromSquare) (pieceMoves state position)

moveGenSquare :: PieceMoveGen -> GameState s -> Square -> ST s [Move]
moveGenSquare pieceMoves state fromSquare = do
    piece <- readArray board fromSquare
    moveGenPosition pieceMoves state (fromSquare, piece)

genericMoveGen :: PieceMoveGen -> GameState s -> ST s [Move]
genericMoveGen pieceMoves state@(GameState turn turnColor board)
    = (concat . sequence) 
        [moveGenPosition pieceMoves state position
        | position@(square, Piece pieceColor _) <- positions board
        , pieceColor == turnColor ]
              
genericMove :: Move -> GameState s -> ST s (GameState s)
genericMove (fromSquare, toSquare) (GameState turn turnColor board)
    = GameState turn' (invertColor turnColor) (board // [(fromSquare, Blank), (toSquare, movedPiece)])
    where
        movedPiece = board ! fromSquare
        turn' = case turnColor of
                  White -> turn
                  Black -> turn+1

validMove :: PieceMoveGen -> GameState -> Move -> Bool
validMove pieceMoves state@(GameState turn turnColor board) move@(fromSquare, _)
    | movedPiece == Blank     = error ("Move.validMove: no piece at position " ++ (show fromSquare))
    | turnColor /= movedColor = error "Move.validMove: piece does not belong to color on move"
    | not moveValid           = error "Move.validMove: invalid move for piece"
    | otherwise               = True
    where
        movedPiece = board ! fromSquare
        movedColor = colorOf movedPiece
        moveValid = move `elem` (moveGenSquare pieceMoves state fromSquare)
