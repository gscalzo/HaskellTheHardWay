{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

toInt :: String -> Int
toInt xs = read xs

timestamp :: LogMessage -> Int
timestamp (LogMessage (Error _) ts _) = ts
timestamp (LogMessage _ ts _) = ts
timestamp _ = 0

parseMessage :: String -> LogMessage
parseMessage pm = case words pm of
                    ("I" : ts : xs)      -> LogMessage Info (toInt ts) (unwords xs)
                    ("W" : ts : xs)      -> LogMessage Warning (toInt ts) (unwords xs)
                    ("E" : en : ts : xs) -> LogMessage (Error (toInt en)) (toInt ts) (unwords xs)
                    xs                   -> Unknown (unwords xs)

parse :: String -> [LogMessage]
parse content = map parseMessage $ lines content


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf 
insert _ n@(Node _ (Unknown _) _) = n
insert newlm (Node ltree ndelm rtree) = if timestamp newlm < timestamp ndelm
                                            then Node (insert newlm ltree) ndelm rtree
                                            else Node ltree ndelm (insert newlm rtree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (msg:xs) = case msg of
                            (LogMessage (Error en) _ smsg) -> if en > 50
                                                                then [smsg] ++ whatWentWrong xs
                                                                else whatWentWrong xs
                            _ -> whatWentWrong xs

clean :: [LogMessage] -> [String]
clean [] = []
clean (msg:xs) = case msg of
                            (LogMessage (Error _) _ smsg) -> [smsg] ++ clean xs
                            (LogMessage _ _ _) -> clean xs
                            _ -> clean xs





