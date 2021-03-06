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

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x `div` 10 + x `mod` 10 + (sumDigits xs)

validate :: Integer -> Bool
validate l = 0 == sumDigits (doubleEveryOther (toDigits l)) `mod` 10

