import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Tree
import Havoc.Components.OpeningBook
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Game.MiniChess
import Havoc.Notation
import System.IO

doGenBook :: (Game a) => a RealWorld -> Int -> Score -> IO OpeningBook
doGenBook state depth scoreRange = do
    putStrLn $ "Generating opening book with depth " ++ show depth ++ "..."
    putStr "["

    book <- genBook status state depth scoreRange
    
    putStrLn "]"
    return book 
    where
	cs = "0123456789"
	ccount = length cs
        status curDepth
            | diff == 0                       = putChar $ head cs
            | diff < ccount && curDepth >= 7  = putChar $ cs !! diff
            | otherwise                       = return ()
            where diff = depth - curDepth
            
main = do
    hSetBuffering stdout NoBuffering
    start <- stToIO $ startState :: IO (MiniChess RealWorld)
    bounds <- stToIO $ getBounds $ (board . gameState) start

    let depth = 13
        range = 4
    book <- doGenBook start depth range
    saveBook ("openingbook-" ++ show depth ++ "-" ++ show range) book
    --putStr $ drawForest $ fmap (fmap ((showMove bounds) . (decodeMove bounds))) book
