intersperse :: a -> [[a]] -> [a]
intersperse a [] = []
intersperse a (x:[]) = x
intersperse a (x:xs) = x ++ a:(intersperse a xs)