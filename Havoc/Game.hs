module Havoc.Game where

import Control.Monad.ST
import Havoc.Game.Chesslike.Move
import Havoc.Game.Chesslike.State

data Result = Win Color
            | Draw
            deriving (Show, Eq)

data GameStatus = End Result
                | Continue [Move]
                deriving (Show, Eq)
            
class Game a where
    gameStatus :: a s -> ST s GameStatus
    moveGen    :: a s -> ST s [Move]
    move       :: Move -> a s -> ST s (a s, MoveDiff)
    startState :: a s
    evaluate   :: a s -> GameStatus -> ST s Int
    
    showState  :: a s -> ST s String
    readState  :: String -> ST s (a s)

max_eval_score :: Int
max_eval_score = 9999
