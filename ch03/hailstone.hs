hailstone :: Integer -> [Integer]
hailstone n
        | n <= 0    = []
        | n == 1    = [1]
        | even n    = n : hailstone (n `div` 2)  
        | otherwise = n : hailstone (n * 3 + 1)

hailstoneLength :: Integer -> Int
hailstoneLength = length . hailstone 

