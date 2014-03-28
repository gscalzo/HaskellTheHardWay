module Main where

import LogAnalysis
import Log

main :: IO ()
main = do
    --ms <-  testParse parse 10000 "Sample.log"
    ms <- testWhatWentWrong parse whatWentWrong "Sample.log"
    --ms <- testWhatWentWrong parse clean "Sample.log"
    --print ms
    mapM_ print ms
    --let aa = inOrder $ build ms
    ----mapM_ print aa
    --print $ whatWentWrong aa
