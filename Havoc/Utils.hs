module Havoc.Utils where

import Control.Monad.ST.Strict
import Data.Time.Clock (NominalDiffTime)
import Data.List (unfoldr)
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
        
minimumsPair :: (Ord a) => [(a,b)] -> [(a,b)]
minimumsPair [] = []
minimumsPair ls
    = filter ((==mina) . fst)
    $ ls
    where
        mina = (minimum . map fst) ls
        
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
