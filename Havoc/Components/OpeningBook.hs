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
type OpeningBook = (BoardBounds, Int, Forest EncodedMove)

emptyBook :: OpeningBook
emptyBook = (((0,0),(0,0)), 0, [])

encodeMove :: BoardBounds -> Move -> EncodedMove
encodeMove (lB, uB) move = fromIntegral $ index moveBounds move
    where moveBounds = ((lB,lB), (uB,uB))
    
decodeMove :: BoardBounds -> EncodedMove -> Move
decodeMove (lB, uB) encMove = (range moveBounds) !! (fromIntegral encMove)
    where moveBounds = ((lB,lB), (uB,uB)) 

genBook :: (Game a) => (Int -> IO ()) -> a RealWorld -> Int -> Score -> IO OpeningBook
genBook status state depth scoreRange = do
    nodeCount <- newIORef 0
    bounds <- stToIO $ getBounds $ (board . gameState) state
    
    (_, moveForest) <- negamaxPrunedTreeStatus status state nodeCount depth scoreRange (-max_eval_score) (max_eval_score)
    
    when checkBookGen (putStr $ drawForest $ fmap (fmap (\(Evaluated v m) -> show v ++ " " ++ showMove bounds m)) moveForest)
    
    let encode = (encodeMove bounds) . stripEvaluated
    let tree = (flip unfoldForest) (sortForest moveForest)
                 (\(Node move f) -> (encode move, sortForest f))
                 
    return (bounds, depth, tree)
    where 
        sortForest = reverse . sortBy (comparing (scoreOf . rootLabel))

advanceBook :: OpeningBook -> Move -> OpeningBook
advanceBook (bounds, bookDepth, book) move
    = (bounds, bookDepth-1, book')
    where
        encMove = encodeMove bounds move
        book' = case find (\(Node encMove' _) -> encMove' == encMove) book of
                  Nothing                 -> []
                  Just (Node _ subForest) -> subForest

bookMove :: OpeningBook -> Maybe (Move, OpeningBook)
bookMove (_, _, []) = Nothing
bookMove (bounds, bookDepth, book)
    = Just (topMove, (bounds, bookDepth-1, subForest topNode))
    where
        topNode = head book
        topMove = (decodeMove bounds) . rootLabel $ topNode
        
bookMoveCutoff :: Int -> OpeningBook -> Maybe (Move, OpeningBook)
bookMoveCutoff depth book = if (bookDepth book <= depth) then Nothing else bookMove book
        
bookDepth :: OpeningBook -> Int
bookDepth (bounds, depth, book) = depth
        
topLine :: OpeningBook -> [Move]
topLine book = unfoldr bookMove book

printBook :: OpeningBook -> IO ()
printBook (bounds, bookDepth, book) = putStr $ drawForest $ fmap (fmap ((showMove bounds) . (decodeMove bounds))) book

saveBook :: FilePath -> OpeningBook -> IO ()
saveBook = encodeFile

loadBook :: FilePath -> IO OpeningBook
loadBook = decodeFile
