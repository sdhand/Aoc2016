import Data.Ord
import Data.List

top5 x = take 5 $ nub $ reverse $ sortBy (comparing $ count $ reverse $ sort x) $ reverse $ sort x
    where count = (\x y -> length ( filter (y==) x))

valid :: String -> Int
valid x = if top5 room == checksum then read sectorId else 0
    where splitted = splitAt ((length x)-7) x
          room = filter (\x -> x /= '-') $ fst $ splitAt ((length x)-10) $ fst splitted
          sectorId = snd $ splitAt ((length x)-10) $ fst splitted
          checksum = tail $ init $ snd splitted

main = do
    input <- readFile "input.txt"
    print $ sum $ map valid $ lines input
