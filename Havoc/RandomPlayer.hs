import Random
import Data.List
import Havoc.State
import Havoc.UI
import Havoc.MiniChess.Move
import Havoc.MiniChess.Game

randomChoice xs g = (xs !! index, g')
    where (index, g') = randomR (0, (length xs)-1) g

randomMove state g
    | null moves  = (Nothing, g')
    | otherwise   = (Just (move m state), g')
    where
        moves = moveGen state
        (m, g') = randomChoice moves g

main = do seed <- getStdRandom random
          let g = mkStdGen seed
          let states = unfoldr nextMove (startState, g)
          putStr (unlines (map show states))
          
       where
           nextMove (state, g)
               | isGameOver state  = Nothing
               | otherwise         = let (m, g') = randomMove state g in
                                         case m of
                                             Nothing -> Nothing
                                             Just s  -> Just (s, (s, g'))
