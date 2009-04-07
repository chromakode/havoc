module Havoc.UI where

import Data.Char
import Havoc.State
import Havoc.MiniChess.Move

decodeCoord :: String -> Square
decodeCoord (alphaCol:numRow) = (row,column)
    where
        row    = (read numRow) - 1
        column = ord alphaCol - ord 'a'
        
decodeMove :: String -> Move
decodeMove moveStr = (decodeCoord fromCoord, decodeCoord toCoord)
    where
        (fromCoord, '-':toCoord) = span (/='-') moveStr

humanMove :: String -> State -> State
humanMove moveStr state 
    | validMove state m  = move m state
    | otherwise          = error "UI.humanMove: invalid move specified"
    where m = decodeMove moveStr
        
