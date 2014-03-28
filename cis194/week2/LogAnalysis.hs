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
parse content = map parseMessage (lines content)
