toDigits :: Integer -> [Integer]
toDigits x
		| x <= 0 	= []
		| otherwise = (toDigits (x `div` 10)) ++ ((x `mod` 10) : [])

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
        | x <= 0    = []
        | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))        

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l = reverse (doubleEveryOther' (reverse l) )

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' [x,y] = [x, (2 * y)]
doubleEveryOther' (x:y:zs) = x : (2 * y) : (doubleEveryOther' zs)

