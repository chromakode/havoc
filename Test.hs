import Control.Monad
import Control.Monad.ST
import Random
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Game.MiniChess
import Havoc.Player
import Havoc.Player.DoUndo
import Havoc.Player.Negamax
import Havoc.Player.NegamaxPruned
import Debug.Trace

startBoardText =
    "kqbnr\n\
    \ppppp\n\
    \.....\n\
    \.....\n\
    \PPPPP\n\
    \RNBQK\n"

countMoveGen state = do
    count <- stToIO $ countMoves state 2
    putStrLn $ "Node count: " ++ show count
    where
        countMoves :: (Game a) => a s -> Int -> ST s Int
        countMoves state' 0     = return 1
        countMoves state' depth = do
            sts <- showGameState (gameState state')
            return $! trace (sts) 0
            moves <- moveGen state'
            return $! trace (show moves) 0
            counts <- mapMoves state' (\m s -> countMoves state' (depth-1)) moves
            return $ sum counts

testMoveScoring state = do
    moves <- stToIO $ moveGen state
    (stToIO $ (showGameState . gameState) state) >>= putStrLn
    (stToIO $ showMoves state moves) >>= putStrLn
    
    stdGen <- getStdGen
    sortedMoves <- stToIO $ shuffleAndSortStatuses stdGen state moves
    (stToIO $ showMoves state sortedMoves) >>= putStrLn
    
    putStrLn "---"
    
    (nodes, scoredMoves) <- stToIO $ negamaxMoves state 2
    (stToIO $ showScoredMoves state scoredMoves) >>= putStrLn
    putStrLn (show nodes)
    
    
main = do
    state <- stToIO $ readBoard startBoardText >>= (\board -> return $ MiniChess $ Evaluated 0 $ GameState 1 White board)
    countMoveGen state
    testMoveScoring state
