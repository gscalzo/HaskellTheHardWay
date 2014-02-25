evenSum :: [Integer] -> Integer
evenSum l = accumSum 0 l

accumSum n l = if l == []
                  then n
                  else let x = head l
                           xs = tail l
                       in if even x
                             then accumSum (n+x) xs
                             else accumSum n xs
                                    

evenSum' :: [Integer] -> Integer
evenSum' [] = 0
evenSum' (x:xs) = if even x then x + evenSum' xs else evenSum' xs     