import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Binary
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

showTopLine filePath = do
    start <- stToIO $ startState :: IO (MiniChess RealWorld)
    book@(bounds, _, _) <- loadBook filePath
    let line = topLine book
    putStrLn $ show (map (showMove bounds) line)
    
convertOldMCBook filePath = do
    start <- stToIO $ startState :: IO (MiniChess RealWorld)
    bounds <- stToIO $ getBounds $ (board . gameState) start
    
    forest <- decodeFile filePath :: IO (Forest EncodedMove)
    let depth = maximum $ map (length . levels) forest
    saveBook (filePath++"-converted") (bounds, depth, forest)
    putStrLn $ "Converted book of depth " ++ show depth ++ "."
            
makeMiniChessBook = do
    hSetBuffering stdout NoBuffering
    start  <- stToIO $ startState :: IO (MiniChess RealWorld)

    let depth = 13
        range = 4
    book <- doGenBook start depth range
    saveBook ("openingbook-" ++ show depth ++ "-" ++ show range) book
    
main = convertOldMCBook "openingbook"
