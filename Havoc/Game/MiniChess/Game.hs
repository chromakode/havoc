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

newtype MiniChess s = MiniChess (Evaluated (GameState s))
instance Game MiniChess where
    startState = mcStartBoard >>= (\board -> return $ MiniChess $ Evaluated 0 $ GameState 1 White board)

    gameStatus mcState@(MiniChess (Evaluated value state)) = do
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

    moveGen        (MiniChess    (Evaluated v s))       = chessMoveGen mcMoves s
    validMove      (MiniChess    (Evaluated v s))       = chessValidMove mcMoves s
    doMove         (MiniChess es@(Evaluated v s)) move  = mcMove es move >>= (\(es', d) -> return (MiniChess es', d))
    undoMove       (MiniChess es@(Evaluated v s)) ediff = (chessUndoMoveEval es ediff) >>= return . MiniChess
    evaluateResult (MiniChess es@(Evaluated v s))       = mcEvaluateResult s
    score          (MiniChess    (Evaluated v s))       = case turnColor s of
                                                            White -> return v
                                                            Black -> return (-v)
    
    copyState      (MiniChess    (Evaluated v s))       = copyGameState s >>= (\s' -> return $ MiniChess $ Evaluated v $ s')
    gameState      (MiniChess    (Evaluated v s))       = s
