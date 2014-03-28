{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage pm = case words pm of
                    ("I" : ts : xs)    -> LogMessage Info (read ts) (unwords xs)
                    ("W" : ts : xs)    -> LogMessage Warning (read ts) (unwords xs)
                    ("E" : en : ts : xs) -> LogMessage (Error (read en)) (read ts) (unwords xs)
                    xs                  -> Unknown (unwords xs)
