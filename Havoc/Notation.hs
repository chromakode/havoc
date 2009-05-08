module Havoc.Notation where

import Data.Array
import Data.Char
import Havoc.State
import Havoc.Move

readCoord :: BoardBounds -> String -> Square
readCoord ((li,lj),(ui,uj)) (alphaCol:numRow)
    | row < li    || row > ui    = error "Notation.readCoord: row out of bounds"
    | column < lj || column > uj = error "Notation.readCoord: column out of bounds"
    | otherwise                  = (row,column)
    where
        row    = ui - (read numRow) + 1
        column = ord alphaCol - ord 'a'
        
readMove :: BoardBounds -> String -> Move
readMove bbounds moveStr = (readCoord bbounds fromCoord, readCoord bbounds toCoord)
    where
        (fromCoord, toCoord)
            = case span (/='-') moveStr of
                (fc, '-':tc) -> (fc, tc)
                _            -> error "Notation.decodeMove: unable to read move"

readMove' :: State -> String -> Move
readMove' state = readMove ((bounds . board) state)

showCoord :: BoardBounds -> Square -> String
showCoord ((li,lj),(ui,uj)) (i,j) = column : row
    where
        row    = show $ ui - i + 1
        column = chr $ j + (ord 'a')

showMove :: BoardBounds -> Move -> String
showMove bbounds (fromSquare, toSquare) = (showCoord bbounds fromSquare) ++ "-" ++ (showCoord bbounds toSquare)

showMove' :: State -> Move -> String
showMove' state = showMove ((bounds . board) state)
