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
import Havoc.Negamax
import Havoc.Notation
import Havoc.UI
import Havoc.Utils
import Havoc.MiniChess.Move
import Havoc.MiniChess.Game
import Havoc.MiniChess.Evaluate

type Player = State -> IO Move

-- Player definitions

randomChoice xs = do
    r <- getStdRandom index
    return (xs !! r)
    where index = randomR (0, (length xs)-1)

mcNegamaxMovesID = negamaxMovesID gameStatus evaluate move 2

mcNegamaxMove :: Player
mcNegamaxMove state = do
    moves <- mcNegamaxMovesID state
    randomChoice moves
                         
mcHumanMove :: Player
mcHumanMove state = do 
    line <- readline "Your move: "
    case line of
      Nothing   -> do putStrLn "quit."; exitWith ExitSuccess
      Just text -> catch (return $! humanMove miniChessMoves text state)
                         (\e -> do putStrLn (show e)
                                   mcHumanMove state)

players = [("human",   mcHumanMove)
          ,("negamax", mcNegamaxMove)]
          
--- Game loop

play :: Player -> Player -> (String -> IO()) -> State -> IO State
play whitePlayer blackPlayer log state
    = case (gameStatus state) of
        status@(End _)       -> do gameOver state status; return state
        Continue state moves -> do putStrLn (show state)
                                   m <- playerMove turnColor
                                   log $ showMove m
                                   let newstate = move m state
                                   play whitePlayer blackPlayer log newstate
    where
        playerMove color = case (turnColor state) of 
                             White -> whitePlayer state
                             Black -> blackPlayer state
                               
gameOver state status = do 
    putStrLn (show state)
    putStrLn (explainStatus status)

-- Program and command line argument stuff

data Options = Options { help        :: Bool
                       , list        :: Bool
                       , logName     :: Maybe String
                       , whitePlayer :: Player
                       , blackPlayer :: Player }

defaultOptions = Options { help        = False
                         , list        = False
                         , logName     = Nothing
                         , whitePlayer = mcHumanMove
                         , blackPlayer = mcNegamaxMove }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['w'] ["white"]   (ReqArg (readPlayer White) "PLAYER")     "White player type (default: human)"
    , Option ['b'] ["black"]   (ReqArg (readPlayer Black) "PLAYER")     "Black player type (default: negamax)"
    , Option ['l'] ["log"]     (OptArg ((\name opts -> opts { logName = Just name })
                                       . fromMaybe "game") "FILE")      "Log file name"
    , Option []    ["list"]    (NoArg (\opts -> opts { list = True }))  "List player types"
    , Option ['h'] ["help"]    (NoArg (\opts -> opts { help = True }))  "This help"
    ]
header = "Usage: play"

showHelp = putStrLn (usageInfo header options)

showPlayers = putStrLn $ "Available player types: " ++ (intercalate ", " (map fst players))

readPlayer color text
    = case lookup text players of
        Just player -> case color of
                         White -> (\opts -> opts { whitePlayer = player })
                         Black -> (\opts -> opts { blackPlayer = player })
        Nothing     -> error $ "Invalid player \"" ++ text ++ "\"specified" 
        
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
        startPlay log = play (whitePlayer opts) (blackPlayer opts) log startState
        noLog = (\_ -> return ())
