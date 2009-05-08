module Havoc.UI where

import Data.Array
import Havoc.Game
import Havoc.Move
import Havoc.Notation
import Havoc.State

humanMove :: PieceMoveGen -> String -> State -> Move
humanMove pieceMoves moveStr state 
    | validMove pieceMoves state m  = m
    | otherwise          = error "UI.humanMove: invalid move specified"
    where m = readMove' state moveStr

explainStatus :: Status -> String
explainStatus (End _ (Win color)) = (colorName color) ++ " wins."
explainStatus (End _ Draw)        = "The game is a draw."
explainStatus (Continue state _)  = (colorName (turnColor state)) ++ " to move."
