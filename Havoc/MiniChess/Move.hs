module Havoc.MiniChess.Move where

import Havoc.State

data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest
type Move (Square, Square)

line :: Square -> Direction -> Square
line square direction = undefined
    
moves :: Position -> [Move]
moves ((i,j), Pawn) = undefined

moveGen :: State -> [Move]
moveGen state = concat [moves position | position <- pieces (board state)]
