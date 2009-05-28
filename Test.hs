import Control.Monad
import Control.Monad.ST
import Random
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Game.MiniChess.Game
import Havoc.Player
import Havoc.Player.DoUndo
import Havoc.Player.NegamaxPruned

startBoardText =
    "k....\n\
    \....p\n\
    \...P.\n\
    \.....\n\
    \p....\n\
    \.P..K\n"

testMoveGen = do
    state <- stToIO $ readBoard startBoardText >>= (\board -> return $ MiniChess $ Evaluated 0 $ GameState 2 Black board)
    moves <- stToIO $ moveGen state
    (stToIO $ (showGameState . gameState) state) >>= putStrLn
    (stToIO $ showMoves state moves) >>= putStrLn
    
    stdGen <- getStdGen
    sortedMoves <- stToIO $ shuffleAndSortStatuses stdGen state moves
    (stToIO $ showMoves state sortedMoves) >>= putStrLn
    
    (nodes, scoredMoves) <- negamaxPrunedMove state 1
    (stToIO $ showScoredMoves state scoredMoves) >>= putStrLn
    
    
    
main = testMoveGen
