-- 
-- Copyright (c) 2014 Giordano Scalzo  
-- The MIT License (MIT)
-- 
import Control.Monad
import System.Random 

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

main :: IO ()
main = do
    mapM_ putStrLn intro
    res <- playGame
    putStrLn res


playGame = do
	putStrLn "Try again? (yes or no)"
	word <- getLine
	if word == "no"
		then return ""
	else do
		money <- playHand 100
		return $ show money

playHand money = do
	putStrLn $ show money
	putStrLn "What is your bet?"
	bet <- getLine
	let (valid, prompt) = validBet (read bet) money
	putStrLn prompt
	let deck = fromIntegralList [2..14]
	--if valid
	--	then 
	--		return (read (playHand money))
	gen <- getStdGen 
	let (randNumber, newGen) = randomR (0, length deck) gen :: (Int, StdGen) 
	let (f, deck1) = getCard randNumber deck

	let (randNumber, newGen) = randomR (0, length deck1) gen :: (Int, StdGen) 
	let (s, deck2) = getCard 2 deck1

	let (randNumber, newGen) = randomR (0, length deck2) gen :: (Int, StdGen) 
	let (t, deck3) = getCard 2 deck2

	let resultString = " " ++ (cardToString f) ++ " "++ (cardToString s) ++ " "++ (cardToString t) ++ " "
	putStrLn resultString
		--else putStrLn prompt
	let result = money - read (bet)
	if result <= 0
		then
			return result
		else do
			money <- playHand result
			return money


validBet :: Int -> Int -> (Bool, [Char])
validBet bet money 
		| bet <= 0 = (False, "Chicken")
		| bet > money = (False, "Sorry, my friend but you have bet to much.\n" ++ "You have only " ++ show (bet) ++ " dollars to bet.")
	 	| otherwise = (True, "Go for it, dude!")


getCard :: Int -> [Int] -> (Int, [Int])
getCard idx deck = (deck !! idx, removeCard idx deck)
					where removeCard i d = 
						let (ys, zs) = splitAt i d 
								in ys ++ tail zs

fromIntegralList :: [Integer] -> [Int]
fromIntegralList = map (fromIntegral)

cardToString :: Int -> [Char]
cardToString card
	| card < 11 = show card
	| card == 11 = "Jack"
	| card == 12 = "Queen"
	| card == 13 = "King"
	| otherwise = "Ace"








