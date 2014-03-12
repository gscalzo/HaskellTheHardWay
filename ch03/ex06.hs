
import Data.Ord
import Data.List

subLSort :: [[a]] -> [[a]]
subLSort list = sortBy (comparing length) list

subLSort' :: [[a]] -> [[a]]
subLSort' list = sortBy compareLength list
                 where compareLength a b = compare (length a) (length b)