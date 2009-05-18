module Havoc.Player where

import Data.List
import Data.Time.Clock
import Text.Printf
import Havoc.Move
import Havoc.Notation
import Havoc.State

data PlayerResult = PlayerResult { searchStats :: Maybe (Int,Int)
                                 , playerMove  :: Move            }

type Player = State -> IO PlayerResult
type PlayerDebug = (String -> IO ()) -> Player

data Timed a = Timed { time        :: NominalDiffTime 
                     , timedResult :: a               }

timedIO :: IO a -> IO (Timed a)
timedIO action = do
    t1 <- getCurrentTime
    r  <- action
    t2 <- getCurrentTime
    return $ Timed (diffUTCTime t2 t1) r

timedPlayer :: Player -> State -> IO (Timed PlayerResult)
timedPlayer player state = do timedIO (player state)

showScoredMoves :: State -> [(Double, Move)] -> String
showScoredMoves state moves
    = intercalate " | "
    . map (\(s,m) -> (printf "%+.2f : " s) ++ (showMove' state m))
    $ moves
