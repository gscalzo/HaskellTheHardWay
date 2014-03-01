-- 
-- Copyright (c) 2014 Giordano Scalzo  
-- The MIT License (MIT)
-- 
import System.Environment
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



