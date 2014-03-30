module Golf where

skips :: [a] -> [[a]]
skips l = map (\(x,y) -> skip (x, y)) $ fullIndexed l  

indexed :: [a] -> [(a, Int)]
indexed l = l `zip` [1..]

fullIndexed :: [a] -> [([a], Int)]
fullIndexed l = map (\(x,y) -> (l, y))$ indexed l 

-- skip ([1,2,3,4,5,6], 2) -> [2,4,6]
-- skip ([1,2,3,4,5,6], 3) -> [3,6]
skip :: ([a] , Int) -> [a]
skip (l,n) = [fst x | x <- l `zip` [1..], (snd x) `mod` n == 0]
