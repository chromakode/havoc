module Havoc.Game.MiniChess.Game where

import Havoc.State
import Havoc.Game
import Havoc.Game.MiniChess.Move

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
            
startState = State 1 White startBoard

gameStatus :: State -> Status
gameStatus state
    | isDeadKing = End state (Win remainingKingColor)
    | isDraw     = End state Draw
    | otherwise  = Continue state moves
    where
        kings = filter ((King==) . pieceType)
              $ pieces (board state)
              
        isDeadKing = (length kings) == 1
        remainingKingColor = (colorOf . head) kings

        isDraw = (turn state) > 40
               || null moves
        
        moves = moveGen state
