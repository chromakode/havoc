module Havoc.Utils where

import Data.Array.MArray
import Control.Monad.ST.Strict
import Data.IORef
import Data.List (unfoldr)
import Data.Time.Clock (NominalDiffTime)
import Data.STRef.Strict
import Numeric

minimumsBy :: (Ord b) => (a -> b) -> [a] -> [a]
minimumsBy p [] = []
minimumsBy p as
    = map fst
    . filter ((==minb) . snd) 
    $ abs
    where 
        abs = map (\a -> (a, p a)) as
        minb = (minimum . map snd) abs

selectWithBy :: (Eq b) => (a -> b) -> ([b] -> b) -> [a] -> [a]
selectWithBy with by [] = []
selectWithBy with by ls
    = filter ((==selected) . with)
    $ ls
    where
        selected = (by . map with) ls
   
minimumsPair :: (Ord a) => [(a,b)] -> [(a,b)]
minimumsPair = selectWithBy fst minimum
        
maximumsPair :: (Ord a) => [(a,b)] -> [(a,b)]
maximumsPair = selectWithBy fst maximum

minimums, maximums :: (Ord a) => [a] -> [a]
minimums = selectWithBy id minimum
maximums = selectWithBy id maximum
        
-- Take alternate items from a list of lists
-- E.g. [[1,2], [3,4]] => [1,3,2,4]
stripe :: [[a]] -> [a]
stripe =  concat . (unfoldr next)
       where next b = if (not (null b'))
                          then Just (map head b', map tail b')
                          else Nothing
                    where b' = filter (not . null) b

showSeconds :: Int -> NominalDiffTime -> String
showSeconds digits duration = showFFloat (Just digits) seconds ""
    where
        -- This is pretty dumb. 
        seconds = (fromRational . toRational) duration
        
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = (\x -> writeSTRef ref $! f x) =<< readSTRef ref

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = readIORef ref >>= (\x -> writeIORef ref $! f x)

copyMArray :: (Ix i, MArray a e m) => a i e -> m (a i e)
copyMArray array = do
    bounds <- getBounds array
    elems  <- getElems array
    newListArray bounds elems
    
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
