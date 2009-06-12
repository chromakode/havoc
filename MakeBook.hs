import Control.Monad
import Control.Monad.ST
import Havoc.Components.OpeningBook
import Havoc.Game
import Havoc.Game.MiniChess
import System.IO

doGenBook :: (Game a) => a RealWorld -> Int -> IO OpeningBook
doGenBook state depth = do
    putStrLn $ "Generating opening book with depth " ++ show depth ++ "..."
    putStr "["

    book <- genBook status state depth
    
    putStrLn "]"
    return book 
    where
        status curDepth = do
            case curDepth of
                _| curDepth == depth     -> putStr "+"
                _| curDepth == (depth-1) -> putStr "-"
                _| curDepth >= 6         -> putStr "."
                otherwise                -> return ()
            
main = do
    hSetBuffering stdout NoBuffering
    start <- stToIO $ startState :: IO (MiniChess RealWorld)
    book <- doGenBook start 10
    saveBook "openingbook" book
