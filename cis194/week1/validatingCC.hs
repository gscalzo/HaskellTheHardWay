toDigits :: Integer -> [Integer]
toDigits x
		| x <= 0 	= []
		| otherwise = (toDigits (x `div` 10)) ++ ((x `mod` 10) : [])

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
        | x <= 0    = []
        | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))        