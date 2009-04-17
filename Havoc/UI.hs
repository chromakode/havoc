module Havoc.UI where

import Data.Char
import Havoc.Game
import Havoc.State
import Havoc.Move

decodeCoord :: String -> Square
decodeCoord (alphaCol:numRow) = (row,column)
    where
        row    = (read numRow) - 1
        column = ord alphaCol - ord 'a'
        
decodeMove :: String -> Move
decodeMove moveStr = (decodeCoord fromCoord, decodeCoord toCoord)
    where
        (fromCoord, '-':toCoord) = span (/='-') moveStr

humanMove :: PieceMoveGen -> String -> State -> Move
humanMove pieceMoves moveStr state 
    | validMove pieceMoves state m  = m
    | otherwise          = error "UI.humanMove: invalid move specified"
    where m = decodeMove moveStr

explainStatus :: Status -> String
explainStatus (End (Win color))  = (colorName color) ++ " wins."
explainStatus (End Draw)         = "The game is a draw."
explainStatus (Continue state _) = (colorName (turnColor state)) ++ " to move."
