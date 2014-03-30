module Golf where

skips :: [a] -> [[a]]
skips l = map skip . map (\(x,y) -> (l, y)) $ idx l  

idx :: [a] -> [(a, Int)]
idx l = l `zip` [1..]

skip :: ([a] , Int) -> [a]
skip (l,n) = [fst x | x <- idx l, (snd x) `mod` n == 0]
