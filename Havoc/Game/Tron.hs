module Havoc.Game.Tron where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Utils (copyMArray, swap)
import Debug.Trace

--
-- Move Generation
--
            
tronMoves :: GameState s -> Position -> ST s [Square]
tronMoves state (square, Piece _ Player) = dirMoves [North .. West] state square
tronMoves state (square, Wall)           = error "Move.tronMoves: moves for wall square requested"
tronMoves state (square, Blank)          = error "Move.tronMoves: moves for blank square requested"

tronMove :: MiniChess s -> Move -> ST s (MiniChess s, Evaluated MoveDiff)
tronMove (MiniChess eS@(Evaluated oldValue state) pF) move@(fromSquare, toSquare) = do
    -- Evaluate the previous state
    undoDelta <- tronEvaluatePreMove state pF move
    
    -- Perform the move
    (newState, diff) <- tronDoMove state move
    
    -- Evaluate the new state
    newValue <- tronEvaluateMove newState pF diff oldValue undoDelta
    return (MiniChess (Evaluated newValue newState) pF, Evaluated oldValue diff)
    where
        printPawnFiles = do
            elems <- getElems pF
            st <- showGameState state
            return $! trace (st ++ show elems ++ "\n---") ()

tronUndoMove :: MiniChess s -> Evaluated MoveDiff -> ST s (MiniChess s)
tronUndoMove (MiniChess eS@(Evaluated v s) pF) eDiff@(Evaluated _ diff) = do
    eS' <- tronUndoMoveEval eS eDiff
    return $ MiniChess eS' pF

--
-- Evaluation
-- 

winScore :: GameState s -> Score
winScore (GameState turn turnColor board) = max_eval_score - (turn * 2)

tronEvaluateResult :: GameState s -> Result -> ST s Score
tronEvaluateResult state@(GameState turn turnColor board) result
    = case result of
        Draw        -> return 0
        Win color   -> let sign = if color == turnColor then 1 else -1 in
                       return $ sign * (winScore state)

colorSign, lastColorSign :: Color -> Int
colorSign color = case color of
                    Self -> 1
                    Enemy -> -1

lastColorSign = colorSign . invertColor

tronEvaluatePreMove :: GameState s -> PawnFiles s -> Move -> ST s Score
tronEvaluatePreMove state@(GameState turn turnColor board) pF (fromSquare, toSquare) = do
    movedPiece <- readArray board fromSquare
    takenPiece <- readArray board toSquare
    movedPastScore <- positionScore state pF (fromSquare, movedPiece)
    return $ -movedPastScore

tronEvaluateMove :: GameState s -> PawnFiles s -> MoveDiff -> Score -> Score -> ST s Score
tronEvaluateMove state pawnFiles diff@(MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) oldValue undoDelta = do
    -- In the MiniChess type, scores are stored with static sign, with positive values in White's favor, and negative values in Black's favor. In this module, scores are treated with relative sign, with positive scores in the current player's favor. We convert the sign scheme here.
    let sign = lastColorSign (turnColor state)
    if takenPiece /= Blank && (pieceType takenPiece) == Player
      then return $ sign * (winScore state)
      else do positionDelta <- positionScore state pawnFiles (toSquare, becomePiece)
              return $ oldValue + (sign * (positionDelta + undoDelta))
        
positionScore :: GameState s -> PawnFiles s -> Position -> ST s Score
positionScore state pawnFiles          (_     , Blank) = return 0
positionScore state pawnFiles position@(square@(row,col), piece@(Piece color pieceType)) = do
    return 0

--
-- Basic rules and Game instance
--

tronStartBoard :: ST s (Board s)
tronStartBoard = readBoard tronStartBoardText
    where
        tronStartBoardText = 
            "kqbnr\n\
            \ppppp\n\
            \.....\n\
            \.....\n\
            \PPPPP\n\
            \RNBQK\n"

type PawnFiles s = STUArray s (Color,Int) Int
data MiniChess s = MiniChess { tronState   :: Evaluated (GameState s) }
instance Game MiniChess where
    {-# SPECIALIZE instance Game MiniChess #-}
    fromBoard board = return $ MiniChess (Evaluated 0 $ GameState board)
        
    startState = tronStartBoard >>= fromBoard

    gameStatus tronState@(MiniChess (Evaluated value state) pF) = do
        moves <- moveGen tronState
        let isDead = null moves
            
          
        if isDraw
          then return $ End Draw
          else return $ Continue moves

    moveGen        (MiniChess    (Evaluated v s) pF)       = tronMoveGen tronMoves s
    pieceMoveGen   (MiniChess    (Evaluated v s) pF)       = moveGenPosition tronMoves s
    validMove      (MiniChess    (Evaluated v s) pF)       = tronValidMove tronMoves s
    doMove         tron                                move  = tronMove tron move
    undoMove       tron                                eDiff = tronUndoMove tron eDiff
    evaluateResult (MiniChess eS@(Evaluated v s) pF)       = tronEvaluateResult s
    score          (MiniChess    (Evaluated v s) pF)       = case turnColor s of
                                                               Self -> return v
                                                               Enemy -> return (-v)
    
    copyState      (MiniChess    (Evaluated v s) pF)       = do gameState' <- copyGameState s
                                                                pawnFiles' <- copyMArray pF
                                                                return $ MiniChess (Evaluated v gameState') pawnFiles'
    gameState      (MiniChess    (Evaluated v s) pF)       = s
