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

startBoardText =
    "kqbnr\n\
    \ppppp\n\
    \.....\n\
    \.....\n\
    \PPPPP\n\
    \RNBQK\n"

countMoveGen state = do
    count <- stToIO $ countMoves state 4
    putStrLn $ "Node count: " ++ show count
    where
        countMoves :: (Game a) => a s -> Int -> ST s Int
        countMoves state' 0     = return 1
        countMoves state' depth = do
            moves <- moveGen state'
            counts <- mapMoves state' (\m s -> countMoves state' (depth-1)) moves
            return $ sum counts

testMoveScoring state = do
    moves <- stToIO $ moveGen state
    (stToIO $ (showGameState . gameState) state) >>= putStrLn
    (stToIO $ showMoves state moves) >>= putStrLn
    
    sortedMoves <- stToIO $ sortMoves state moves
    (stToIO $ showMoves state sortedMoves) >>= putStrLn
    
    putStrLn "---"
    
    (depth, nodes, scoredMoves) <- negamaxPrunedMovesID putStrLn 7 state
    (stToIO $ showScoredMoves state scoredMoves) >>= putStrLn

runTurn mover state seconds = do
    (depth, nodes, scoredMoves) <- mover putStrLn seconds state
    (stToIO $ showScoredMoves state scoredMoves) >>= putStrLn   
    
main = do
    state <- stToIO $ readBoard startBoardText >>= fromBoard :: IO (MiniChess RealWorld)
    runTurn negamaxMovesID       state 10
    runTurn negamaxPrunedMovesID state 10
