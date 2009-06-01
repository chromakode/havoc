import Prelude hiding (catch)

import Control.Exception hiding (evaluate)
import Control.Monad
import Control.Monad.ST
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
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Notation
import Havoc.Player
import Havoc.Player.IterativeDeepening
import Havoc.Player.Negamax
import Havoc.Player.NegamaxPruned
import Havoc.UI
import Havoc.Utils
import Havoc.Game.MiniChess

-- Player definitions

randomChoice xs = do
    r <- getStdRandom index
    return (xs !! r)
    where index = randomR (0, (length xs)-1)

mcRandomMove :: PlayerDebug (MiniChess RealWorld)
mcRandomMove debugLn state = do
    moves <- stToIO $ moveGen state
    move <- randomChoice moves
    return $ PlayerResult (Just (1, 1)) move

mcNegamaxMove' mover debugLn state = do
    (depth, nodes, moves) <- mover debugLn 7 state
    scores <- stToIO $ showScoredMoves state moves
    debugLn $ "Choosing from moves: " ++ scores
    (s, m) <- randomChoice moves
    return $ PlayerResult (Just (depth, nodes)) m

mcNegamaxMove       = mcNegamaxMove' negamaxMovesID
mcNegamaxPrunedMove = mcNegamaxMove' negamaxPrunedMovesID
     
mcHumanMove :: PlayerDebug (MiniChess RealWorld)
mcHumanMove debugLn state = do 
    line <- readline "Your move: "
    case line of
      Nothing   -> do putStrLn "quit."; exitWith ExitSuccess
      Just text -> catch       (do move <- stToIO $ humanMove state text
                                   return $! PlayerResult Nothing move)
                         (\e -> do putStrLn (show (e :: IOError))
                                   mcHumanMove debugLn state)
                        
mcIOMove :: PlayerDebug (MiniChess RealWorld)
mcIOMove debugLn state = do
    hFlush stdout
    line <- getLine
    let text = fromMaybe line (stripPrefix "! " line)
    move <- stToIO $ humanMove state text
    return $! PlayerResult Nothing move

data PlayerType = Human | IO | Random | Negamax | NegamaxPruned
    deriving (Show, Read, Eq, Enum)

playerFor :: PlayerType -> PlayerDebug (MiniChess RealWorld)
playerFor Human         = mcHumanMove
playerFor IO            = mcIOMove
playerFor Random        = mcRandomMove
playerFor Negamax       = mcNegamaxMove
playerFor NegamaxPruned = mcNegamaxPrunedMove

--- Game loop

play :: PlayerType -> PlayerType -> (String -> IO()) -> Bool -> MiniChess RealWorld -> IO (MiniChess RealWorld)
play whitePlayer blackPlayer logLn debug state = do
    status <- stToIO $ gameStatus state
    case status of
        status@(End _) -> do (stToIO $ (showGameState . gameState) state) >>= putStrLn
                             if hasIO
                               then putStr "= "
                               else return ()
                             putStrLn $ explainStatus state status
                             return state
                                   
        Continue moves -> do let curPlayer = (playerOfColor . turnColor . gameState) state
                                 oppIsIO   = (playerOfColor . invertColor . turnColor . gameState) state == IO
                             
                             (stToIO $ (showGameState . gameState) state) >>= putStrLn
                             putStrLn $ show curPlayer ++ " moving..."
                             
                             Timed dt (PlayerResult stats m) <- timedPlayer (playerMove curPlayer) state
                                   
                             debugLn $ "Player returned move"
                                     ++ (maybe "" (\(depth,nodes)
                                                   -> (" of depth " ++ (show depth)
                                                    ++ " (" ++ (show nodes) ++ " nodes)"))
                                        stats)
                                     ++ " after " ++ (show dt)
                             (stToIO $ showMove' state m) >>= logLn
                             when oppIsIO $
                                (stToIO $ showMove' state m) >>= putStrLn . ("! "++)
                             putStrLn ""
                             
                             (newstate, diff) <- stToIO $ doMove state m
                             play whitePlayer blackPlayer logLn debug newstate
    where
        playerOfColor color
            = case color of
                White -> whitePlayer
                Black -> blackPlayer
                
        hasIO = whitePlayer == IO || blackPlayer == IO
                
        playerMove player = playerFor player debugLn
                
        debugLn text
            | debug == True  = do putStr "[debug] "; putStrLn text
            | otherwise      = return ()

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
               endStatus <- stToIO $ gameStatus endState
               log $ explainStatus endState endStatus
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
        startPlay log = stToIO startState >>= play (whitePlayer opts) (blackPlayer opts) log (debug opts)
        noLog = (\_ -> return ())
