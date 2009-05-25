module Havoc.Game.Chesslike.MiniChess.Game where

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
instance MCState Game where
    gameStatus :: MCState s -> ST s Status
    gameStatus mcState@(MCState state) = do
        ps <- pieces state
        
        let king = filter ((King==) . pieceType)
            isDeadKing = (length kings) == 1
            remainingKingColor = (colorOf . head) kings
            
        if isDeadking
          then return $ End state (Win remainingKingColor)
          else do moves <- moveGen mcState
                  let isDraw = (turn state) > 40
                             || null moves
                  
                  if isDraw
                    then return $ End Draw
                    else return $ Continue moves

    moveGen (MCState state) = genericMoveGen mcMoves state
    move (MCState state)    = MCState . mcMove
    
    startState = MCState 1 White mcStartBoard
    evaluate   = mcEvaluate
    
    showState  = showState
    readState  = readState
