-- | Main entry point to the application.
module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _         _       _ = []
hanoi 1 sourcePeg destPeg _ = [(sourcePeg, destPeg)]
hanoi n sourcePeg destPeg tempPeg = hanoi (n - 1) sourcePeg tempPeg destPeg ++ [(sourcePeg, destPeg)] ++ hanoi (n - 1) tempPeg destPeg sourcePeg

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _         _       _         _ = []
hanoi' 1 sourcePeg destPeg _         _ = [(sourcePeg, destPeg)]
hanoi' n p1 p2 p3  rest = hanoi' k p1 p3 p2 rest ++ 
                          hanoi' (n - k) p1 p2 rest p3 ++
                          hanoi' k p3 p2 p1 rest
                          where k = n `div` 2 



-- hanoi for n disks and r pegs [p1, p2, ..., pr]
hanoiR :: Integer -> [a] -> [(a, a)]

-- zero disks: no moves needed.
hanoiR 0 _ = []

-- one disk: one move and two pegs needed.
hanoiR 1 (p1 : p2 : rest) = [(p1, p2)] -- only needed for smart-alecks?


-- n disks and r > 3 pegs: use Frame-Stewart algorithm
hanoiR n (p1 : p2 : p3 : rest) =
    hanoiR k (p1 : p3 : p2 : rest) ++
    hanoiR (n - k) (p1 : p2 : rest) ++
    hanoiR k (p3 : p2 : p1 : rest)
    where k
            | (null rest)   = n - 1
            | otherwise     = n `div` 2                                                

main = do
    numOfDiscs <- readLn :: IO Integer
    --print $ length $ hanoi numOfDiscs "1" "2" "3"
    --print $ length $ hanoi' numOfDiscs "1" "2" "3" "4"
    print $ hanoi' numOfDiscs "a" "b" "c" "d"
    print $ hanoiR numOfDiscs ['a', 'b', 'c', 'd']