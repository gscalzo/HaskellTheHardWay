-- 
-- Copyright (c) 2014 Giordano Scalzo  
-- The MIT License (MIT)
-- 
import System.Environment

main :: IO ()
main = getArgs >>= print . haqify . head
 
haqify s = "Haq! " ++ s
