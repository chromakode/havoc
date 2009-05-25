module Havoc.Notation where

import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Havoc.Game.State
import Havoc.Game.Move

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

readMove' :: GameState s -> String -> ST s Move
readMove' state text = (getBounds . board) state >>= (\bounds -> return $ readMove bounds text)

showCoord :: BoardBounds -> Square -> String
showCoord ((li,lj),(ui,uj)) (i,j) = column : row
    where
        row    = show $ ui - i + 1
        column = chr $ j + (ord 'a')

showMove :: BoardBounds -> Move -> String
showMove bbounds (fromSquare, toSquare) = (showCoord bbounds fromSquare) ++ "-" ++ (showCoord bbounds toSquare)

showMove' :: GameState s -> Move -> ST s String
showMove' state move = (getBounds . board) state >>= (\bounds -> return $ showMove bounds move)
