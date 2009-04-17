import Random
import Data.List
import Havoc.Game
import Havoc.State
import Havoc.UI
import Havoc.Move
import Havoc.MiniChess.Move
import Havoc.MiniChess.Game

randomChoice xs g = (xs !! index, g')
    where (index, g') = randomR (0, (length xs)-1) g

randomMove state moves g = (move m state, g')
    where
        (m, g') = randomChoice moves g

nextMove (state, g)
   = case (gameStatus state) of
         End result       -> Nothing
         Continue _ moves -> let (s, g') = randomMove state moves g in
                             Just (s, (s, g'))

main = do seed <- getStdRandom random
          let g = mkStdGen seed
          let states = unfoldr nextMove (startState, g)
          putStr (unlines (map show states))
          putStrLn ((explainStatus . gameStatus . last) states)
