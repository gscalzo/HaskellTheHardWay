{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

toInt :: String -> Int
toInt xs = read xs

parseMessage :: String -> LogMessage
parseMessage pm = case words pm of
                    ("I" : ts : xs)    -> LogMessage Info (toInt ts) (unwords xs)
                    ("W" : ts : xs)    -> LogMessage Warning (toInt ts) (unwords xs)
                    ("E" : en : ts : xs) -> LogMessage (Error (toInt en)) (toInt ts) (unwords xs)
                    xs                  -> Unknown (unwords xs)

parse :: String -> [LogMessage]
parse content = map parseMessage $ lines content


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf 
insert _ n@(Node _ (Unknown _) _) = n
insert newlm@(LogMessage _ mts _) (Node ltree ndelm@(LogMessage _ nts _) rtree) = if mts < nts
                                                                                        then Node (insert newlm ltree) ndelm rtree
                                                                                        else Node ltree ndelm (insert newlm rtree)


build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right