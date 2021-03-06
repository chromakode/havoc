{-# LANGUAGE BangPatterns #-}

module Havoc.Game.Move where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Ix (inRange)
import Data.List (union)
import Havoc.Game.State

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest deriving (Show, Eq, Enum)
type Move = (Square, Square)
data MoveType = Move | Capture | MoveCapture | XRay | Friendly | IsPiece Piece
data MoveDiff = MoveDiff Piece Move Piece Piece deriving Show
type PieceMoveGen s = GameState s -> Position -> ST s [Square]

type Score = Int
data Evaluated a = Evaluated Score a
                 deriving (Eq, Show)

dirMove :: Direction -> Square -> Square
dirMove North     !(!i,!j) = (i-1,j)
dirMove South     !(!i,!j) = (i+1,j)
dirMove East      !(!i,!j) = (i,j+1)
dirMove West      !(!i,!j) = (i,j-1)
dirMove Northeast !(!i,!j) = (i-1,j+1)
dirMove Southeast !(!i,!j) = (i+1,j+1)
dirMove Southwest !(!i,!j) = (i+1,j-1)
dirMove Northwest !(!i,!j) = (i-1,j-1)

canCapture :: GameState s -> Square -> ST s Bool
canCapture state = (liftM not) . (isTurnColor state)

isFriendly :: GameState s -> Square -> ST s Bool
isFriendly state = isTurnColor state

validPointMove :: MoveType -> GameState s -> Square -> ST s Bool
validPointMove capture state@(GameState turn turnColor board) square = do
    bounds <- getBounds board
    if (inRange bounds square)
      then case capture of
             XRay        -> return True
             Move        -> isBlank board square
             
             Capture     -> do blank   <- isBlank board square
                               capture <- canCapture state square
                               return $ not blank && capture
                                
             MoveCapture -> do blank   <- isBlank board square
                               capture <- canCapture state square
                               return $ blank || capture
                                
             Friendly    -> do blank  <- isBlank board square
                               friend <- isFriendly state square
                               return $ not blank && friend
                               
             IsPiece p   -> do piece <- readArray board square
                               return $ piece == p
      else return False

validPointMoves :: MoveType -> GameState s -> [Square] -> ST s [Square]
validPointMoves capture state = filterM (validPointMove capture state)

dirMoves :: MoveType -> [Direction] -> GameState s -> Square -> ST s [Square]
dirMoves capture directions state square
    = validPointMoves capture state
    $ map (`dirMove` square) directions

lineMove :: MoveType -> Direction -> GameState s -> Square -> ST s [Square]
{-# INLINE lineMove #-}
lineMove capture direction state@(GameState turn turnColor board) square = do
    bounds <- getBounds board
    untilBlocked state
        . takeWhile (inRange bounds)
        . drop 1
        $ iterate (dirMove direction) square
    where
        untilBlocked :: GameState s -> [Square] -> ST s [Square]
        untilBlocked state [] = return []
        untilBlocked state@(GameState turn turnColor board) (s:squares) = case capture of
            XRay -> do
                continue $ Just s
        
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
                    else return $ addIf capture s
            
            MoveCapture -> do
                blank   <- isBlank board s
                capture <- canCapture state s
                if blank
                    then continue $ Just s
                    else return $ addIf capture s
                    
            Friendly -> do
                blank    <- isBlank board s
                friendly <- isFriendly state square
                if blank
                    then continue Nothing
                    else return $ addIf friendly s
            
            where
                continue (Just s) = untilBlocked state squares >>= (\tail -> return (s : tail))
                continue Nothing  = untilBlocked state squares
                addIf b s = if b then [s] else []

lineMoves :: MoveType -> [Direction] -> GameState s -> Square -> ST s [Square]
lineMoves capture directions state square
    = (liftM concat) $ mapM (\d -> lineMove capture d state square) directions

knightMoves :: MoveType -> GameState s -> Square -> ST s [Square]
knightMoves capture state square@(i,j)
    = validPointMoves capture state
    $ concat [[(i+da,j+db), (i+db,j+da)] | da <- [-2,2], db <- [-1,1]]
           
(+..+) :: ST s [Square] -> ST s [Square] -> ST s [Square]
mover1 +..+ mover2 = do
    moves1 <- mover1
    moves2 <- mover2
    return $ union moves1 moves2

moveGenPosition :: PieceMoveGen s -> GameState s -> Position -> ST s [Move]
moveGenPosition pieceMoves state position@(fromSquare, _) = do
    moves <- pieceMoves state position
    return $ map ((,) fromSquare) moves

moveGenSquare :: PieceMoveGen s -> GameState s -> Square -> ST s [Move]
moveGenSquare pieceMoves state@(GameState turn turnColor board) fromSquare = do
    piece <- readArray board fromSquare
    moveGenPosition pieceMoves state (fromSquare, piece)

chessMoveGen :: PieceMoveGen s -> GameState s -> ST s [Move]
chessMoveGen pieceMoves state@(GameState turn turnColor board) = do
    pos <- positions board
    ((liftM concat) . sequence) 
        [moveGenPosition pieceMoves state position
        | position@(square, Piece pieceColor _) <- pos
        , pieceColor == turnColor ]

chessDoMove :: GameState s -> Move -> ST s (GameState s, MoveDiff)
chessDoMove (GameState turn turnColor board) move@(fromSquare, toSquare) = do 
    movedPiece <- readArray board fromSquare
    takenPiece <- readArray board toSquare
    writeArray board fromSquare Blank
    writeArray board toSquare movedPiece
    let newState = GameState turn' (invertColor turnColor) board
        undo     = MoveDiff movedPiece move takenPiece movedPiece
    return (newState, undo)
    where
        turn' = case turnColor of
                  White -> turn
                  Black -> turn+1

chessUndoMove :: GameState s -> MoveDiff -> ST s (GameState s)
chessUndoMove (GameState turn turnColor board) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece _) = do 
    writeArray board fromSquare movedPiece
    writeArray board toSquare takenPiece
    return $ GameState turn' (invertColor turnColor) board
    where
        turn' = case turnColor of
                  White -> turn-1
                  Black -> turn
                  
chessUndoMoveEval :: Evaluated (GameState s) -> Evaluated MoveDiff -> ST s (Evaluated (GameState s))
chessUndoMoveEval (Evaluated _ state) (Evaluated oldValue diff) = do 
    oldState <- chessUndoMove state diff
    return $ Evaluated oldValue oldState

chessValidMove :: PieceMoveGen s -> GameState s -> Move -> ST s Bool
chessValidMove pieceMoves state@(GameState turn turnColor board) move@(fromSquare, _) = do
    movedPiece <- readArray board fromSquare
    moves <- moveGenSquare pieceMoves state fromSquare
    return $ canMove movedPiece moves
    where
        canMove movedPiece moves
            | movedPiece == Blank     = error ("Move.validMove: no piece at position " ++ (show fromSquare))
            | turnColor /= movedColor = error "Move.validMove: piece does not belong to color on move"
            | not (move `elem` moves) = error "Move.validMove: invalid move for piece"
            | otherwise               = True
            where 
                movedColor = colorOf movedPiece
