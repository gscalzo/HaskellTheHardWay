module Golf where

skips :: [a] -> [[a]]
skips l = map skip . map (\(x,y) -> (l, y)) $ idx l  

idx :: [a] -> [(a, Int)]
idx l = l `zip` [1..]

skip :: ([a] , Int) -> [a]
skip (l,n) = [fst x | x <- idx l, (snd x) `mod` n == 0]


localMaxima :: [Integer] -> [Integer]
localMaxima l = concat . map lm $ triplets l 

triplets :: [a] -> [[a]]
triplets [] = []
triplets [x] = []
triplets [x,y] = []
triplets (x:y:z:xs) = [[x,y,z]] ++ triplets (y:z:xs)

lm :: [Integer] -> [Integer]
lm [x,y,z] = if y > x && y > z
            then [y]
            else []