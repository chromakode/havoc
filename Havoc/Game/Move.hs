{-# LANGUAGE BangPatterns #-}

module Havoc.Game.Move where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Ix (inRange)
import Data.List (union)
import Havoc.Game.State

data Direction = North | East | South | West deriving (Show, Eq, Enum)
type Move = (Square, Square)
data MoveType = Move | Capture | MoveCapture | XRay | Friendly | IsPiece Piece
data MoveDiff = MoveDiff Piece Move Piece Piece deriving Show
type PieceMoveGen s = GameState s -> Position -> ST s [Square]

type Score = Int
data Evaluated a = Evaluated { scoreOf        :: Score
                             , stripEvaluated :: a     }
                 deriving (Eq, Show)

dirMove :: Direction -> Square -> Square
dirMove North     !(!i,!j) = (i-1,j)
dirMove South     !(!i,!j) = (i+1,j)
dirMove East      !(!i,!j) = (i,j+1)
dirMove West      !(!i,!j) = (i,j-1)

validPointMove :: MoveType -> GameState s -> Square -> ST s Bool
validPointMove capture state@(GameState turn turnColor board) square = do
    bounds <- getBounds board
    if (inRange bounds square)
      then squareIs isBlank board square
      else return False

validPointMoves :: MoveType -> GameState s -> [Square] -> ST s [Square]
validPointMoves capture state = filterM (validPointMove capture state)

dirMoves :: MoveType -> [Direction] -> GameState s -> Square -> ST s [Square]
dirMoves capture directions state square
    = validPointMoves capture state
    $ map (`dirMove` square) directions

moveGenPosition :: PieceMoveGen s -> GameState s -> Position -> ST s [Move]
moveGenPosition pieceMoves state position@(fromSquare, _) = do
    moves <- pieceMoves state position
    return $ map ((,) fromSquare) moves

moveGenSquare :: PieceMoveGen s -> GameState s -> Square -> ST s [Move]
moveGenSquare pieceMoves state@(GameState turn turnColor board) fromSquare = do
    piece <- readArray board fromSquare
    moveGenPosition pieceMoves state (fromSquare, piece)

tronMoveGen :: PieceMoveGen s -> GameState s -> ST s [Move]
tronMoveGen pieceMoves state@(GameState turn turnColor board) = do
    pos <- positions board
    ((liftM concat) . sequence) 
        [moveGenPosition pieceMoves state position
        | position@(square, Piece pieceColor _) <- pos
        , pieceColor == turnColor ]

tronDoMove :: GameState s -> Move -> ST s (GameState s, MoveDiff)
tronDoMove (GameState turn turnColor board) move@(fromSquare, toSquare) = do 
    movedPiece <- readArray board fromSquare
    takenPiece <- readArray board toSquare
    writeArray board fromSquare Blank
    writeArray board toSquare movedPiece
    let newState = GameState turn' (invertColor turnColor) board
        undo     = MoveDiff movedPiece move takenPiece movedPiece
    return (newState, undo)
    where
        turn' = case turnColor of
                  Self -> turn
                  Enemy -> turn+1

tronUndoMove :: GameState s -> MoveDiff -> ST s (GameState s)
tronUndoMove (GameState turn turnColor board) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece _) = do 
    writeArray board fromSquare movedPiece
    writeArray board toSquare takenPiece
    return $ GameState turn' (invertColor turnColor) board
    where
        turn' = case turnColor of
                  Self -> turn-1
                  Enemy -> turn
                  
tronUndoMoveEval :: Evaluated (GameState s) -> Evaluated MoveDiff -> ST s (Evaluated (GameState s))
tronUndoMoveEval (Evaluated _ state) (Evaluated oldValue diff) = do 
    oldState <- tronUndoMove state diff
    return $ Evaluated oldValue oldState
