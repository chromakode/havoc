module Havoc.Game.Chesslike.MiniChess.Game where

import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.Chesslike.State
import Havoc.Game.Chesslike.Move
import Havoc.Game.Chesslike.MiniChess.Evaluate
import Havoc.Game.Chesslike.MiniChess.Move

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

newtype MCState s = MCState (GameState s)
instance Game MCState where
    gameStatus mcState@(MCState state) = do
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

    moveGen (MCState state)   = genericMoveGen mcMoves state
    move (MCState state) move = mcMove state move >>= (\(s, d) -> return (MCState s, d))
    evaluate (MCState state)  = mcEvaluate state
    
    startState = mcStartBoard >>= (\board -> return $ MCState (GameState 1 White board))
    
    showState (MCState state) = showGameState state
    readState = (liftM MCState) . readGameState
