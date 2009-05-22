module Havoc.Utils where

import Data.List (unfoldr)

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

