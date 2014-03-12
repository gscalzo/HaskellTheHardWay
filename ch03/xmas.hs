import Data.Time

main = do
    t <- getCurrentTime
    print $ diffDays (fromGregorian 2014 12 25) (utctDay t)
