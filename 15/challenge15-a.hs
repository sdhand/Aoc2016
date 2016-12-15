import Data.List

type Disc = Int -> Bool

makeDisc :: Int -> Int -> Disc
makeDisc positions start = (== 0).(`mod` positions).(+ start)

time :: [Disc] -> Int -> Int
time discs t = if and $ zipWith ($) discs [(t+1)..] then t else time discs (t+1)

main = do
    input <- readFile "input.txt"
    print $ time (map (\x -> makeDisc (read $ x !! 3) (read $ init $ x !! 11)) $ map words $ lines input) 0
