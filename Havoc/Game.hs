module Havoc.Game where

import Havoc.Move
import Havoc.State

data Result = Win Color
            | Draw
            deriving (Show, Eq)

data Status = End State Result
            | Continue State [Move]
            deriving (Show, Eq)
