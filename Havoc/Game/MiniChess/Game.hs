module Havoc.Game.MiniChess.Game where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Game.MiniChess.Evaluate
import Havoc.Game.MiniChess.Move

mcStartBoard :: ST s (Board s)
mcStartBoard = readBoard mcStartBoardText
    where
        mcStartBoardText = 
            "kqbnr\n\
            \ppppp\n\
            \.....\n\
            \.....\n\
            \PPPPP\n\
            \RNBQK\n"

newtype MiniChess s = MiniChess (EvaluatedState s)
instance Game MiniChess where
    startState = mcStartBoard >>= (\board -> return $ MiniChess (GameState 1 White board))

    gameStatus mcState@(MiniChess state) = do
        ps <- pieces (board state)
        
        let kings = filter ((King==) . pieceType) ps
            isDeadKing = (length kings) == 1
            remainingKingColor = (colorOf . head) kings

        if isDeadKing
          then return $ End (Win remainingKingColor)
          else do moves <- moveGen mcState
                  let isDraw = (turn state) > 40
                             || null moves
                  
                  if isDraw
                    then return $ End Draw
                    else return $ Continue moves

    moveGen      (MiniChess    (EvaluatedState v s))      = chessMoveGen mcMoves s
    validMove    (MiniChess    (EvaluatedState v s))      = chessValidMove mcMoves s
    doMove       (MiniChess    (EvaluatedState v s)) move = mcMove es move >>= (\(es', d) -> return (MiniChess es', d))
    undoMove     (MiniChess es@(EvaluatedState v s)) diff = (chessUndoMove es diff) >>= return . MiniChess
    evaluateMove (MiniChess es@(EvaluatedState v s))      = mcEvaluateMove es
    score        (MiniChess    (EvaluatedState v s))      = value
    
    copyState (MiniChess state)     = copyGameState state >>= return . MiniChess
    gameState (MiniChess state)     = state
