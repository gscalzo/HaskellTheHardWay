
module Golf where

import Data.List

skips :: [a] -> [[a]]
skips l = let idx l = l `zip` [1..] in
            let skip (l,n) = [fst x | x <- idx l, (snd x) `mod` n == 0] in
                map skip . map (\(x,y) -> (l, y)) $ idx l  

localMaxima' :: [Integer] -> [Integer]
localMaxima' l = concat . map lm $ trp l 
                where lm [x,y,z] = if y > x && y > z
                                    then [y]
                                    else []

trp :: [a] -> [[a]]
trp [] = []
trp [x] = []
trp [x,y] = []
trp (x:y:z:xs) = [[x,y,z]] ++ trp (y:z:xs)

localMaxima :: [Integer] -> [Integer]
localMaxima l = concat . map (\[x,y,z] -> if y > x && y > z then [y] else []) $ [x | x <- map (take 3) $ tails l, length x == 3]
