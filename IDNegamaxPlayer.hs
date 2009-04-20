import Random
import Data.List
import Havoc.Game
import Havoc.Move
import Havoc.State
import Havoc.UI
import Havoc.Utils
import Havoc.MiniChess.Move
import Havoc.MiniChess.Game
import Havoc.MiniChess.Evaluate
import Havoc.Negamax

mcNegamaxMovesID = negamaxMovesID gameStatus evaluate move 1

randomChoice xs = do r <- getStdRandom index
                     return (xs !! r)
    where index = randomR (0, (length xs)-1)

play state 
    = case (gameStatus state) of
        status@(End result) -> gameOver status
        Continue _ moves    -> do moves <- mcNegamaxMovesID state
                                  m <- randomChoice moves
                                  let newstate = move m state
                                  putStrLn (show newstate)
                                  play newstate
                               
gameOver result = putStrLn (explainStatus result)

main = play startState
