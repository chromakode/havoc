module Havoc.Utils where

minimumsBy :: (Ord b) => (a -> b) -> [a] -> [a]
minimumsBy p [] = []
minimumsBy p as
    = map fst
    . filter ((==minb) . snd) 
    $ abs
    where 
        abs = map (\a -> (a, p a)) as
        minb = (minimum . map snd) abs