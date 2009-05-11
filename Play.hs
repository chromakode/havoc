import Prelude hiding (catch)

import Control.Exception hiding (evaluate)
import Data.List
import Data.Maybe
import Data.Time.LocalTime
import Data.Time.Format
import Random
import System
import System.Console.GetOpt
import System.Console.Readline
import System.Directory
import System.IO
import System.Locale
import Havoc.Game
import Havoc.Move
import Havoc.State
import Havoc.Notation
import Havoc.Player
import Havoc.Player.IterativeDeepening
import Havoc.Player.Negamax
import Havoc.Player.NegamaxPruned
import Havoc.UI
import Havoc.Utils
import Havoc.Game.MiniChess.Move
import Havoc.Game.MiniChess.Game
import Havoc.Game.MiniChess.Evaluate

-- Player definitions

type PlayerDebug = (String -> IO ()) -> Player

randomChoice xs = do
    r <- getStdRandom index
    return (xs !! r)
    where index = randomR (0, (length xs)-1)

mcNegamaxMovesID = iterativelyDeepen (negamaxMoves gameStatus evaluate move) 2

mcNegamaxMove :: PlayerDebug
mcNegamaxMove debugLn state = do
    (depth, nodes, moves) <- mcNegamaxMovesID state
    debugLn $ "Choosing from moves: " ++ (intercalate ", " (map (showMove' state) moves))
    m <- randomChoice moves
    return $ PlayerResult (Just (depth, nodes)) m
    
mcNegamaxPrunedMove :: PlayerDebug
mcNegamaxPrunedMove debugLn state = do
    (depth, nodes, moves) <- negamaxPrunedMoveID gameStatus evaluate move 2 state
    return $ PlayerResult (Just (depth, nodes)) (head moves)
                         
mcHumanMove :: PlayerDebug
mcHumanMove debugLn state = do 
    line <- readline "Your move: "
    case line of
      Nothing   -> do putStrLn "quit."; exitWith ExitSuccess
      Just text -> catch (return $! PlayerResult Nothing $! humanMove miniChessMoves text state)
                         (\e -> do putStrLn (show e)
                                   mcHumanMove debugLn state)

data PlayerType = Human | Negamax | NegamaxPruned
    deriving (Show, Read, Enum)

playerFor :: PlayerType -> PlayerDebug
playerFor Human         = mcHumanMove
playerFor Negamax       = mcNegamaxMove
playerFor NegamaxPruned = mcNegamaxPrunedMove

--- Game loop

play :: PlayerType -> PlayerType -> (String -> IO()) -> Bool -> State -> IO State
play whitePlayer blackPlayer logLn debug state@(State turn turnColor board)
    = case (gameStatus state) of
        status@(End _ _)     -> do gameOver state status; return state
        Continue state moves -> do putStrLn (show state)
                                   putStrLn $ (show . playerOfColor) turnColor ++ " moving..."
                                   Timed dt (PlayerResult stats m) <- timedPlayer (playerMove turnColor) state
                                   debugLn $ "Player returned move"
                                           ++ (maybe "" (\(depth,nodes)
                                                          -> (" of depth " ++ (show depth)
                                                           ++ " (" ++ (show nodes) ++ " nodes)"))
                                              stats)
                                           ++ " after " ++ (show dt)
                                   logLn $ showMove' state m
                                   let newstate = move m state
                                   play whitePlayer blackPlayer logLn debug newstate
    where
        playerOfColor color
            = case color of
                White -> whitePlayer
                Black -> blackPlayer
                
        playerMove color = playerFor (playerOfColor color) debugLn
                
        debugLn text
            | debug == True  = do putStr "[debug] "; putStrLn text
            | otherwise      = return ()
                               
gameOver state status = do 
    putStrLn (show state)
    putStrLn (explainStatus status)

-- Program and command line argument stuff

data Options = Options { help        :: Bool
                       , list        :: Bool
                       , logName     :: Maybe String
                       , debug       :: Bool
                       , whitePlayer :: PlayerType
                       , blackPlayer :: PlayerType   }

defaultOptions = Options { help        = False
                         , list        = False
                         , logName     = Nothing
                         , debug       = False
                         , whitePlayer = Human
                         , blackPlayer = Negamax }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['w'] ["white"]   (ReqArg (readPlayer White) "PLAYER")     ("White player type (default: " ++ (show . whitePlayer) defaultOptions  ++ ")")
    , Option ['b'] ["black"]   (ReqArg (readPlayer Black) "PLAYER")     ("Black player type (default: " ++ (show . blackPlayer) defaultOptions  ++ ")")
    , Option ['l'] ["log"]     (OptArg ((\name opts -> opts { logName = Just name })
                                       . fromMaybe "game") "FILE")      "Log file name"
    , Option ['d'] ["debug"]   (NoArg (\opts -> opts { debug = True })) "Enable debugging output"
    , Option []    ["list"]    (NoArg (\opts -> opts { list = True }))  "List player types"
    , Option ['h'] ["help"]    (NoArg (\opts -> opts { help = True }))  "This help"
    ]
header = "Usage: play"

showHelp = putStrLn (usageInfo header options)

showPlayers = putStrLn $ "Available player types: " ++ (intercalate ", " (map show [Human ..]))

readPlayer :: Color -> String -> Options -> Options
readPlayer color text
    = case (listToMaybe . reads) text of
        Just (player,_) -> case color of
                             White -> (\opts -> opts { whitePlayer = player })
                             Black -> (\opts -> opts { blackPlayer = player })
        Nothing         -> error $ "Invalid player \"" ++ text ++ "\"specified"
        
getLogFileName logName
    = do t <- getZonedTime
         let dateSuffix = formatTime defaultTimeLocale dateFormat t
         fileName <- getNewFileName (logName ++ '_':dateSuffix)
         return fileName
    where
        dateFormat = "%b-%d-%y"
        
getNewFileName :: String -> IO String
getNewFileName fileName = getFileNameIndex fileName Nothing
    where
        getFileNameIndex fileName index
            = do exists <- doesFileExist indexedName
                 if not exists
                    then return indexedName
                    else getFileNameIndex fileName (Just (maybe 0 (+1) index))
            where indexedName = fileName ++ (maybe "" (('_':) . show) index)

showHeader text
    = do putStrLn text
         putStrLn line
    where line = replicate (length text) '-'

logGame name playWithLog
    = do fileName <- getLogFileName name
         showHeader $ "Logging game to file: " ++ fileName
         withFile fileName WriteMode $ \f ->
            let log = hPutStrLn f in
            do t1 <- getZonedTime
               log $ "Game started: " ++ formatTime defaultTimeLocale "%c" t1
               log $ "---"
               endState <- playWithLog log
               log $ "---"
               log $ explainStatus (gameStatus endState)
               t2 <- getZonedTime
               log $ "Game ended: " ++ formatTime defaultTimeLocale "%c" t2
        
main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
      (o,n,[])   -> start (foldl (flip id) defaultOptions o)
      (_,_,errs) -> do putStr (concat errs); showHelp
      
start opts
    | help opts  = showHelp
    | list opts  = showPlayers
    | otherwise  = case logName opts of
                     Nothing   -> do startPlay noLog; return ()
                     Just name -> logGame name startPlay
    where
        startPlay log = play (whitePlayer opts) (blackPlayer opts) log (debug opts) startState
        noLog = (\_ -> return ())
