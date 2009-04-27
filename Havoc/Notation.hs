module Havoc.Notation where

import Data.Char
import Havoc.State
import Havoc.Move

readCoord :: String -> Square
readCoord (alphaCol:numRow) = (row,column)
    where
        row    = (read numRow) - 1
        column = ord alphaCol - ord 'a'
        
readMove :: String -> Move
readMove moveStr = (readCoord fromCoord, readCoord toCoord)
    where
        (fromCoord, toCoord)
            = case span (/='-') moveStr of
                (fc, '-':tc) -> (fc, tc)
                _            -> error "UI.decodeMove: unable to read move"

showCoord :: Square -> String
showCoord (i,j) = column : row
    where
        row    = show $ i + 1
        column = chr $ i + (ord 'a')

showMove :: Move -> String
showMove (fromSquare, toSquare) = (showCoord fromSquare) ++ "-" ++ (showCoord toSquare)
