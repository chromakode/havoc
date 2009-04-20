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

randomChoice xs g = (xs !! index, g')
    where (index, g') = randomR (0, (length xs)-1) g

minOpponentMove state moves g = randomChoice minStates g
    where       
        states = map ((flip move) state) moves
        minStates = minimumsBy evaluate states
        
nextMove (state, g)
    = case (gameStatus state) of
        End result       -> Nothing
        Continue _ moves -> let (s, g') = minOpponentMove state moves g in
                              Just (s, (s, g'))

main = do seed <- getStdRandom random
          let g = mkStdGen seed
          let states = startState : (unfoldr nextMove (startState, g))
          putStr (unlines (map show states))
          putStrLn ((explainStatus . gameStatus . last) states)
