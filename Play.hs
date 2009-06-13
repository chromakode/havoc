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
import System.Console.Readline
import System.Directory
import System.IO
import System.Locale
import Havoc.Components.OpeningBook
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

type MCPlayer = PlayerDebug (MiniChess RealWorld)

randomChoice xs = do
    r <- getStdRandom index
    return (xs !! r)
    where index = randomR (0, (length xs)-1)

mcRandomMove :: NominalDiffTime -> OpeningBook -> MCPlayer
mcRandomMove _ _ debugLn state = do
    moves <- stToIO $ moveGen state
    move <- randomChoice moves
    return $ PlayerResult (Just (1, 1)) move

type Mover = (String -> IO ()) -> NominalDiffTime -> MiniChess RealWorld -> IO (Int, Int, [(Int, Move)])
mcNegamaxMove' :: Mover -> NominalDiffTime -> OpeningBook -> MCPlayer
mcNegamaxMove' mover remaining book debugLn state = do
    case bookMoveCutoff 7 book of
        Just (m, _) -> do moveText <- stToIO $ showMove' state m
                          debugLn $ "Opening book hit: " ++ moveText
                          return $ PlayerResult (Just (bookDepth book, 0)) m
        
        Nothing     -> do let turnTime = (remaining - 1) / (fromIntegral $ 40 - (turn . gameState $ state) + 1)
                          (depth, nodes, moves) <- mover debugLn turnTime state
                          scores <- stToIO $ showScoredMoves state moves
                          debugLn $ "Choosing from moves: " ++ scores
                          (s, m) <- randomChoice moves
                          return $ PlayerResult (Just (depth, nodes)) m

mcNegamaxMove       = mcNegamaxMove' negamaxMovesID
mcNegamaxPrunedMove = mcNegamaxMove' negamaxPrunedMovesID
     
mcHumanMove :: NominalDiffTime ->  OpeningBook -> MCPlayer
mcHumanMove time book debugLn state = do
    line <- readline "Your move: "
    case line of
      Nothing   -> do putStrLn "quit."; exitWith ExitSuccess
      Just text -> catch       (do move <- stToIO $ humanMove state text
                                   return $! PlayerResult Nothing move)
                         (\e -> do putStrLn (show (e :: ErrorCall))
                                   mcHumanMove time book debugLn state)
                        
mcIOMove :: NominalDiffTime -> OpeningBook -> MCPlayer
mcIOMove time book debugLn state = do
    hFlush stdout
    line <- getLine
    let text = fromMaybe line (stripPrefix "! " line)
    move <- stToIO $ humanMove state text
    return $! PlayerResult Nothing move

data PlayerType = Human | IO | Random | Negamax | NegamaxPruned
    deriving (Show, Read, Eq, Enum)


playerFor :: PlayerType -> (NominalDiffTime -> OpeningBook -> MCPlayer)
playerFor Human         = mcHumanMove
playerFor IO            = mcIOMove
playerFor Random        = mcRandomMove
playerFor Negamax       = mcNegamaxMove
playerFor NegamaxPruned = mcNegamaxPrunedMove

--- Game loop

play :: (PlayerType, NominalDiffTime) -> (PlayerType, NominalDiffTime) -> OpeningBook -> (String -> IO()) -> Bool -> MiniChess RealWorld -> IO (MiniChess RealWorld)
play (curPlayer, cPTime) (nextPlayer, nPTime) book logLn debug state = do
    status <- stToIO $ gameStatus state
    case status of
        status@(End _) -> do (stToIO $ (showGameState . gameState) state) >>= putStrLn
                             if hasIO
                               then putStr "= "
                               else return ()
                             putStrLn $ explainStatus state status
                             return state
                                   
        Continue moves -> do startTime <- getCurrentTime
        
                             (stToIO $ (showGameState . gameState) state) >>= putStrLn
                             (stToIO $ score state) >>= (\s -> debugLn $ "Current board score: " ++ show s)
                             
                             putStrLn $ show curPlayer ++ " moving..."
                             
                             Timed dt (PlayerResult stats m) <- timedPlayer (playerMove curPlayer) state
                                   
                             debugLn $ "Player returned move"
                                     ++ (maybe "" (\(depth,nodes)
                                                   -> (" of depth " ++ (show depth)
                                                    ++ " (" ++ (show nodes) ++ " nodes)"))
                                        stats)
                                     ++ " after " ++ (show dt)
                             (stToIO $ showMove' state m) >>= logLn
                             
                             when (nextPlayer == IO) $
                                (stToIO $ showMove' state m) >>= putStrLn . ("! "++)
                             putStrLn ""
                             
                             (newstate, _) <- stToIO $ doMove state m
                             let book' = advanceBook book m
                             
                             endTime <- getCurrentTime
                             let cPTime' = cPTime - (diffUTCTime endTime startTime)
                             
                             play (nextPlayer, nPTime) (curPlayer, cPTime') book' logLn debug newstate
    where
        hasIO = curPlayer == IO || nextPlayer == IO
                
        playerMove player = playerFor player cPTime book debugLn
                
        debugLn text
            | debug == True  = do putStr "[debug] "; putStrLn text
            | otherwise      = return ()

-- Program and command line argument stuff

data Options = Options { help        :: Bool
                       , list        :: Bool
                       , logName     :: Maybe String
                       , bookPath    :: Maybe FilePath
                       , debug       :: Bool
                       , whitePlayer :: PlayerType
                       , blackPlayer :: PlayerType   }

defaultOptions = Options { help        = False
                         , list        = False
                         , bookPath    = Nothing
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
    , Option []    ["book"]    (ReqArg (\path opts -> opts { bookPath = Just path })
                                                           "PATH")      "Opening book path"
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
        Nothing         -> error $ "Invalid player \"" ++ text ++ "\" specified"
        
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
    | otherwise  = do
        book <- case bookPath opts of
          Nothing   -> return emptyBook
          Just path -> loadBook path
          
        case logName opts of
          Nothing   -> do startPlay book noLog; return ()
          Just name -> logGame name (startPlay book)
    where
        startPlay book log = stToIO startState >>= play ((whitePlayer opts), 5*60) ((blackPlayer opts), 5*60) book log (debug opts)
        noLog = (\_ -> return ())
