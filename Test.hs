import Control.Monad
import Control.Monad.ST
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.MiniChess.Game
import Havoc.Player

testMoveGen = do
    state <- stToIO $ (startState :: ST RealWorld (MiniChess RealWorld))
    moves <- stToIO $ moveGen state
    (stToIO $ (showGameState . gameState) state) >>= putStrLn
    (stToIO $ showMoves state moves) >>= putStrLn
    
main = testMoveGen
