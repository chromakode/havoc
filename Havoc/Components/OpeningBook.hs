module Havoc.Components.OpeningBook where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Binary
import Data.IORef
import Data.Ix
import Data.List
import Data.Ord
import Data.Tree
import Data.Word
import Havoc.Game
import Havoc.Game.Move
import Havoc.Game.State
import Havoc.Globals
import Havoc.Player.NegamaxPruned
import System.IO
import Havoc.Notation

type EncodedMove = Word16
type OpeningBook = Forest EncodedMove

encodeMove :: ((Int,Int), (Int,Int)) -> Move -> EncodedMove
encodeMove (lB, uB) move = fromIntegral $ index moveBounds move
    where moveBounds = ((lB,lB), (uB,uB))
    
decodeMove :: ((Int,Int), (Int,Int)) -> EncodedMove -> Move
decodeMove (lB, uB) encMove = (range moveBounds) !! (fromIntegral encMove)
    where moveBounds = ((lB,lB), (uB,uB)) 

genBook :: (Game a) => (Int -> IO ()) -> a RealWorld -> Int -> Score -> IO OpeningBook
genBook status state depth scoreRange = do
    nodeCount <- newIORef 0
    bounds <- stToIO $ getBounds $ (board . gameState) state
    
    (_, moveForest) <- negamaxPrunedTreeStatus status state nodeCount depth scoreRange (-max_eval_score) (max_eval_score)
    
    when checkBookGen (putStr $ drawForest $ fmap (fmap (\(Evaluated v m) -> show v ++ " " ++ showMove bounds m)) moveForest)
    
    let encode = (encodeMove bounds) . stripEvaluated
    return $ (flip unfoldForest) (sortForest moveForest)
               (\(Node move f) -> (encode move, sortForest f))
    where 
        sortForest = reverse . sortBy (comparing (scoreOf . rootLabel))
        
saveBook :: FilePath -> OpeningBook -> IO ()
saveBook = encodeFile

loadBook :: FilePath -> IO OpeningBook
loadBook = decodeFile
