module Havoc.UI where

import Control.Monad.ST
import Data.Array
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Notation

humanMove :: (Game a) => a s -> String -> ST s Move
humanMove state moveStr = do
    move  <- readMove' state moveStr
    valid <- validMove state move
    if valid
        then return move
        else error "UI.humanMove: invalid move specified"

explainStatus :: (Game a) => a s -> GameStatus -> String
explainStatus state (End (Win color)) = (colorName color) ++ " wins."
explainStatus state (End Draw)        = "The game is a draw."
explainStatus state (Continue _)      = (colorName ((turnColor . gameState) state)) ++ " to move."
