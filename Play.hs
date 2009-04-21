import Prelude hiding (catch)

import Control.Exception hiding (evaluate)
import Data.List
import Data.Maybe
import Random
import System
import System.Console.GetOpt
import System.Console.Readline
import Havoc.Game
import Havoc.Move
import Havoc.State
import Havoc.UI
import Havoc.Utils
import Havoc.MiniChess.Move
import Havoc.MiniChess.Game
import Havoc.MiniChess.Evaluate
import Havoc.Negamax

type Player = State -> IO Move

randomChoice xs = do
    r <- getStdRandom index
    return (xs !! r)
    where index = randomR (0, (length xs)-1)

mcNegamaxMovesID = negamaxMovesID gameStatus evaluate move 2

mcNegamaxMove :: State -> IO Move
mcNegamaxMove state = do
    moves <- mcNegamaxMovesID state
    randomChoice moves
                         
mcHumanMove :: State -> IO Move
mcHumanMove state = do 
    line <- readline "Your move: "
    case line of
      Nothing   -> do putStrLn "quit."; exitWith ExitSuccess
      Just text -> catch (return $! humanMove miniChessMoves text state)
                         (\e -> do putStrLn (show e)
                                   mcHumanMove state)

play whitePlayer blackPlayer state 
    = case (gameStatus state) of
        status@(End result)  -> gameOver state status
        Continue state moves -> do putStrLn (show state)
                                   m <- case (turnColor state) of 
                                          White -> whitePlayer state
                                          Black -> blackPlayer state
                                   let newstate = move m state
                                   play whitePlayer blackPlayer newstate
                               
gameOver state result = do 
    putStrLn (show state)
    putStrLn (explainStatus result)

data Options = Options { help        :: Bool
                       , list        :: Bool
                       , whitePlayer :: Player
                       , blackPlayer :: Player }

defaultOptions = Options { help        = False
                         , list        = False
                         , whitePlayer = mcHumanMove
                         , blackPlayer = mcNegamaxMove }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['w'] ["white"] (ReqArg (readPlayer White) "PLAYER") "White player type"
    , Option ['b'] ["black"] (ReqArg (readPlayer Black) "PLAYER") "Black player type"
    , Option ['l'] ["list"]  (NoArg (\opts -> opts { list = True })) "List player types"
    , Option ['h'] ["help"]  (NoArg (\opts -> opts { help = True })) "This help"
    ]
header = "Usage: play"

showHelp = putStrLn (usageInfo header options)

players = [("human",   mcHumanMove)
          ,("negamax", mcNegamaxMove)]

showPlayers = putStrLn $ "Available player types: " ++ (intercalate ", " (map fst players))

readPlayer color text
    = case lookup text players of
        Just player -> case color of
                         White -> (\opts -> opts { whitePlayer = player })
                         Black -> (\opts -> opts { blackPlayer = player })
        Nothing     -> error $ "Invalid player \"" ++ text ++ "\"specified"
        
main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
      (o,n,[])   -> start (foldl (flip id) defaultOptions o)
      (_,_,errs) -> do putStr (concat errs); showHelp
    
start opts
    | help opts  = showHelp
    | list opts  = showPlayers
    | otherwise  = play (whitePlayer opts) (blackPlayer opts) startState
