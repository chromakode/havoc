module Havoc.Game.MiniChess.Move where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Game.MiniChess.Evaluate

mcMoves :: GameState s -> Position -> ST s [Square]
mcMoves state (square, Piece _ _     King)   = dirMoves MoveCapture [North .. Northwest] state square
mcMoves state (square, Piece _ _     Queen)  = lineMoves MoveCapture [North .. Northwest] state square
mcMoves state (square, Piece _ _     Rook)   = lineMoves MoveCapture [North, East, South, West] state square
mcMoves state (square, Piece _ _     Bishop) = (dirMoves Move [North, East, South, West] state square) +..+ (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
mcMoves state (square, Piece _ _     Knight) = knightMoves state square
mcMoves state (square, Piece _ White Pawn)   = (dirMoves Move [North] state square) +..+ (dirMoves Capture [Northwest, Northeast] state square)
mcMoves state (square, Piece _ Black Pawn)   = (dirMoves Move [South] state square) +..+ (dirMoves Capture [Southwest, Southeast] state square)
mcMoves state (square, Blank) = error "Move.mcMoves: moves for blank square requested"

handlePromotion :: GameState s -> MoveDiff -> ST s (GameState s, MoveDiff)
handlePromotion state@(GameState turn turnColor board pieceMap) diff@(MoveDiff movedPiece@(Piece pieceId pieceColor pieceType) (fromSquare, toSquare) takenPiece _) = do
    let isPawn = pieceType == Pawn

    edge <- endRow pieceColor board
    let isEndRow = (fst toSquare) == edge

    if isPawn && isEndRow
        then do let becomePiece = (Piece pieceId pieceColor Queen)
                writeArray board toSquare becomePiece
                let diff' = MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece
                    pieceMap' = IntMap.insert pieceId (toSquare, becomePiece) pieceMap
                    newState  = GameState turn turnColor board pieceMap'
                return (state, diff')
        else return (state, diff)

mcMove :: Evaluated (GameState s) -> Move -> ST s (Evaluated (GameState s), Evaluated MoveDiff)
mcMove es@(Evaluated oldValue state) move@(fromSquare, toSquare) = do
    (newState, diff) <- chessDoMove state move
    (newState, diff) <- handlePromotion newState diff
    newValue <- mcEvaluateMove oldValue newState diff
    return (Evaluated newValue newState, Evaluated oldValue diff)
