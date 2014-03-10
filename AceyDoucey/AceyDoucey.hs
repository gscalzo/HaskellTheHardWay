-- 
-- Copyright (c) 2014 Giordano Scalzo  
-- The MIT License (MIT)
-- 
import Control.Monad
import System.Random 


main :: IO ()
main = do
    mapM_ putStrLn intro
    res <- playGame
    putStrLn res


playGame = do
	money <- playHand 100
	
	putStrLn "Try again? (yes or no)"
	word <- getLine
	if word == "no"
		then return ""
	else do
		playGame

playHand money = do
	putStrLn "What is your bet?"
	bet <- getLine
	let (valid, prompt) = validBet (read bet) money
	let deck = fromIntegralList [2..14]


	randNumber <- randomCard 111
	let (f, deck1) = getCard randNumber deck

	randNumber <- randomCard 111
	let (s, deck2) = getCard randNumber deck1

	randNumber <- randomCard 111
	let (t, deck3) = getCard randNumber deck2


	let (first, second) = sortCard f s

	let resultString = "First card: " ++ (cardToString first) ++ " Second card: "++ (cardToString second) ++ " Third card: "++ (cardToString t) ++ " "
	putStrLn resultString

	let (currentAmount, prompt) = checkCards first second t money (read bet)
	putStrLn prompt

	if currentAmount <= 0
		then
			return currentAmount
		else do
			money <- playHand currentAmount
			return money

intro = ["Acey Ducey Card Game",
	    "Adapted from a BASIC game from Creative Computing - Morristown, New Jersey.",
	    "",
	    "",
	    "",
	    "Acey Ducey is played in the following manner:",
	    "The dealer (computer) deals two cards face up",
	    "You have an option to bet or not to bet depending",
	    "on whether or not you feel the card will have",
	    "a value between the first two.",
	    "If you do not want to bet, input a 0."]


validBet :: Int -> Int -> (Bool, [Char])
validBet bet money 
		| bet <= 0 = (False, "Chicken")
		| bet > money = (False, "Sorry, my friend but you have bet to much.\n" ++ "You have only " ++ show (bet) ++ " dollars to bet.")
	 	| otherwise = (True, "Go for it, dude!")

getCard :: Int -> [Int] -> (Int, [Int])
getCard idx deck =  (deck !! idx', removeCard idx' deck)
				where idx' = idx `mod` (length deck)

removeCard :: Int -> [Int] -> [Int]
removeCard idx deck = ys ++ tail' zs
					where (ys, zs) = splitAt idx deck

fromIntegralList :: [Integer] -> [Int]
fromIntegralList = map (fromIntegral)

cardToString :: Int -> [Char]
cardToString card
	| card < 11 = show card
	| card == 11 = "Jack"
	| card == 12 = "Queen"
	| card == 13 = "King"
	| otherwise = "Ace"

randomCard :: Int -> IO Int
randomCard range =  getStdRandom (randomR (0, range'))
				where range' = range - 1

tail' :: [a] -> [a]
tail' [] = []
tail' (x:[]) = []
tail' (x:xs) = xs

sortCard :: Int -> Int -> (Int, Int)
sortCard f s
	| f < s = (f, s)
	| otherwise = (s, f)


checkCards :: Int -> Int -> Int -> Int -> Int -> (Int, [Char])
checkCards f s t money bet
	| f < t && t < s = (money + bet, "You Win! Current amount: " ++ show (money + bet))
	| otherwise = (money - bet, "You Lose! Current amount: " ++ show (money + bet))




