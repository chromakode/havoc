import Control.Monad
import Control.Monad.ST
import Havoc.Components.OpeningBook
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.MiniChess
import System.IO

doGenBook :: (Game a) => a RealWorld -> Int -> Score -> IO OpeningBook
doGenBook state depth scoreRange = do
    putStrLn $ "Generating opening book with depth " ++ show depth ++ "..."
    putStr "["

    book <- genBook status state depth scoreRange
    
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
    book <- doGenBook start 10 350
    saveBook "openingbook" book
