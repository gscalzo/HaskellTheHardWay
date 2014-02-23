data Tree a = Node a (Tree a) (Tree a)
            | Empty
						deriving (Show)

cntL :: Tree t -> Int
cntL Empty = 0
cntL (Node n l r) = 1 + max( cntL l )( cntL r )




a = Node 'a' Empty Empty
b = Node 'b' Empty Empty
c = Node 'c' a b
d = Node 'd' Empty Empty
e = Node 'e' c d
