module Havoc.MiniChess.Game where

import Havoc.State
import Havoc.Game
import Havoc.MiniChess.Move

startBoard :: Board
startBoard = readBoard startBoardText
    where
        startBoardText = 
            "kqbnr\n\
            \ppppp\n\
            \.....\n\
            \.....\n\
            \PPPPP\n\
            \RNBQK\n"
            
startState = State 0 White startBoard

gameStatus :: State -> Status
gameStatus state
    | isDraw     = End Draw
    | isDeadKing = End (Win remainingKingColor)
    | otherwise  = Continue state moves
    where
        kings = filter ((King==) . pieceType)
              $ pieces (board state)
              
        isDeadKing = (length kings) == 1
        remainingKingColor = (colorOf . head) kings

        isDraw = (turn state) >= 80
               || null moves
        
        moves = moveGen state
