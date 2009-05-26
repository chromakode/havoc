module Havoc.Game where

import Control.Monad.ST
import Havoc.Game.Move
import Havoc.Game.State

data GameStatus = End Result
                | Continue [Move]
                deriving (Show, Eq)

data Result = Win Color
            | Draw
            deriving (Show, Eq)
            
class Game a where
    startState   :: ST s (a s)

    gameStatus   :: a s -> ST s GameStatus
    moveGen      :: a s -> ST s [Move]
    validMove    :: a s -> Move -> ST s Bool
    doMove       :: a s -> Move -> ST s (a s, MoveDiff)
    undoMove     :: a s -> MoveDiff -> ST s (a s)
    evaluate     :: a s -> GameStatus -> ST s Int
    evaluateFast :: a s -> ST s Int

    copyState    :: a s -> ST s (a s)
    gameState    :: a s -> GameState s

max_eval_score :: Int
max_eval_score = 9999
