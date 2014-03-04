-- 
-- Copyright (c) 2014 Giordano Scalzo  
-- The MIT License (MIT)
-- 
import Control.Monad

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
	if valid
		then putStrLn prompt
		else putStrLn prompt
	let result = money - read (bet)
	return result

validBet :: Int -> Int -> (Bool, [Char])
validBet bet money 
		| bet == 0 = (False, "Chicken")
		| bet > money = (False, "Sorry, my friend but you have bet to much.\n" ++ "You have only " ++ show (bet) ++ " dollars to bet.")
	 	| otherwise = (True, "Go for it, dude!")
