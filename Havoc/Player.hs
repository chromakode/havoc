module Havoc.Player where

import Data.Time.Clock
import Havoc.Move
import Havoc.State

data PlayerResult = PlayerResult { searchStats :: Maybe (Int,Int)
                                 , playerMove  :: Move            }

type Player = State -> IO PlayerResult

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
