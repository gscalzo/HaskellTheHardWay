myProduct :: [Integer] -> Integer
myProduct [] = 0
myProduct (x:xs) = x * product(xs)


myProduct' :: [Integer] -> Integer
myProduct' [] = 0
myProduct' l = foldr(*) 1 l

myStar :: (Num a, Eq a) => a -> a -> a
myStar 0 _ = 0
myStar _ 0 = 0
myStar x y = x * y

myProduct'' :: [Integer] -> Integer
myProduct'' = foldr(myStar) 1
