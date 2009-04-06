module Havoc.MiniChess.Game where

import Havoc.State

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
