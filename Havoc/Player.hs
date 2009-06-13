module Havoc.Player where

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Time.Clock
import Text.Printf
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Notation

data PlayerResult = PlayerResult { searchStats :: Maybe (Int,Int)
                                 , playerMove  :: Move            }
type Player a = a -> IO PlayerResult
type PlayerDebug a = (String -> IO ()) -> Player a

data Timed a = Timed { time        :: NominalDiffTime 
                     , timedResult :: a               }

timedIO :: IO a -> IO (Timed a)
timedIO action = do
    t1 <- getCurrentTime
    r  <- action
    t2 <- getCurrentTime
    return $ Timed (diffUTCTime t2 t1) r

timedPlayer :: Player a -> a -> IO (Timed PlayerResult)
timedPlayer player state = timedIO (player state)

showMoves :: (Game a) => a s -> [Move] -> ST s String
showMoves state moves = do
    smoves <- mapM (showMove' state) moves
    return $ intercalate " | " smoves

showScoredMoves :: (Game a) => a s -> [(Int, Move)] -> ST s String
showScoredMoves state moves = do
    scores <- mapM (\(s,m) -> do
        mtext <- showMove' state m
        return $ printf "%+d : " s ++ mtext
        ) moves
    return $ intercalate " | " scores
