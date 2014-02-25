import Data.List(foldl')

fEvenSum :: [Integer] -> Integer
fEvenSum l = foldl mySum 0 (filter even l)
             where  mySum acc x = acc + x  

myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs	

fEvenSum' :: [Integer] -> Integer
fEvenSum' l = myFoldl mySum 0 (filter even l)
             where  mySum acc x = acc + x  

fEvenSum'' :: [Integer] -> Integer
fEvenSum'' l = foldl (\x y -> x + y) 0 (filter even l)

fEvenSum''' :: [Integer] -> Integer
fEvenSum''' l = foldl (+) 0 (filter even l)

fEvenSum'''' :: [Integer] -> Integer
fEvenSum'''' l = foldl' (+) 0 (filter even l)
