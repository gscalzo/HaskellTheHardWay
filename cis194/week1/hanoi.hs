-- | Main entry point to the application.
module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 sourcePeg destPeg _ = [(sourcePeg, destPeg)]
hanoi n sourcePeg destPeg tempPeg = hanoi (n - 1) sourcePeg tempPeg destPeg ++ [(sourcePeg, destPeg)] ++ hanoi (n - 1) tempPeg destPeg sourcePeg

main = do
    numOfDiscs <- readLn :: IO Integer
    print $ hanoi numOfDiscs "A" "B" "C"
