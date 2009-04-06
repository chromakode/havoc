module Havoc.MiniChess.Move where

import Havoc.State

type Move Square

line from_square to_square = undefined
    
moves :: Position
moves position = undefined

moveGen :: State -> [Move]
moveGen state = concat [moves position | position <- pieces (board state)]
