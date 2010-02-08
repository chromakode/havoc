import Prelude hiding (catch)

import Control.Exception hiding (evaluate)
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Random
import System
import System.Console.GetOpt
import System.Directory
import System.IO
import System.Locale
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Notation
import Havoc.Player
import Havoc.Player.IterativeDeepening
import Havoc.Player.Negamax
import Havoc.Player.NegamaxPruned
import Havoc.UI
import Havoc.Utils
import Havoc.Game.Tron

-- Player definitions

type TronPlayer = PlayerDebug (Tron RealWorld)

type Mover = (String -> IO ()) -> NominalDiffTime -> MiniChess RealWorld -> IO (Int, Int, [(Int, Move)])
mcNegamaxMove' :: Mover -> NominalDiffTime -> TronPlayer
mcNegamaxMove' mover debugLn state = do
    let turnTime = 1
    (depth, nodes, moves) <- mover debugLn turnTime state
    scores <- stToIO $ showScoredMoves state moves
    debugLn $ "Choosing from moves: " ++ scores
    (s, m) <- randomChoice moves
    return $ PlayerResult (Just (depth, nodes)) m

mcNegamaxMove       = mcNegamaxMove' negamaxMovesID
mcNegamaxPrunedMove = mcNegamaxMove' negamaxPrunedMovesID

main = playBot havocBot startingValue

playBot :: ([[Spot]] -> a -> (Move, a)) -> a -> IO ()
playBot bot starting = do
    dimsline <- getLine
    let [w,h] = map readInt (words $ dimsline)
    
    (PlayerResult stats m) <- mcNegamaxPrunedMove (\_ -> return ()) state
    interact ((playTurns bot starting) . lines)

readInt :: String -> Int
readInt a = read a

readSpot '#' = Wall
readSpot ' ' = Blank
readSpot '1' = Player
readSpot '2' = Enemy

makeMove North = "1"
makeMove East = "2"
makeMove South = "3"
makeMove West = "4"

playTurns bot pastValue [] = ""
playTurns bot pastValue str = (makeMove move) ++ "\n" ++ playTurns bot history (drop (h+1) str)
    where [w,h] = map readInt (words $ head str)
          tronMap = map (map readSpot) (take h (tail str))
	  (move, history) = bot tronMap pastValue

data Spot = Wall | Blank | Player | Enemy deriving Eq
data Move = North | East | South | West deriving Eq

startingValue = ()

--testBot :: [[Spot]] -> a -> (Move, a)
me tronMap = (maybe 0 id (findIndex (== Player) (head $ filter (any (== Player)) tronMap)), maybe 0 id (findIndex (any (== Player)) tronMap))

canMove move (x,y) tronMap
    | move == North	= if y == 0 then False else (Blank == ((tronMap !! (y-1)) !! x))
    | move == East	= if x+1 == (length (head tronMap)) then False else (Blank == ((tronMap !! y) !! (x+1)))
    | move == South	= if y+1 == (length tronMap) then False else (Blank == ((tronMap !! (y+1)) !! x))
    | move == West	= if x == 0 then False else (Blank == ((tronMap !! y) !! (x-1)))

havocBot tronMap b = (head possibleMoves, b)
    where possibleMoves = (filter (\a -> canMove a (me tronMap) tronMap) [North, East, South, West]) ++ [North]
