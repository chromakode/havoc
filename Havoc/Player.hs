module Havoc.Player where

import Data.List
import Data.Time.Clock
import Text.Printf
import Havoc.Notation

data PlayerResult = PlayerResult { searchStats :: Maybe (Int,Int)
                                 , playerMove  :: Move            }

type Player = a s -> ST s PlayerResult
type PlayerDebug = (String -> IO ()) -> Player

data Timed a = Timed { time        :: NominalDiffTime 
                     , timedResult :: a               }

timedIO :: IO a -> IO (Timed a)
timedIO action = do
    t1 <- getCurrentTime
    r  <- action
    t2 <- getCurrentTime
    return $ Timed (diffUTCTime t2 t1) r

timedPlayer :: Player -> a s -> IO (Timed PlayerResult)
timedPlayer player state = timedIO (stToIO (player state))

showScoredMoves :: State -> [(Int, Move)] -> String
showScoredMoves state moves
    = intercalate " | "
    . map (\(s,m) -> (printf "%+d : " s) ++ (showMove' state m))
    $ moves
