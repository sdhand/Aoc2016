import Text.Printf
import Data.List

input=1364

office :: (Int, Int) -> Bool
office (x, y) = even $ length $ filter (=='1') $ printf "%b" result
    where result = x*x + 3*x + 2*x*y + y + y*y + input

moves :: (Int, Int) -> [(Int, Int)]
moves (x, y) = filter office $ filter valid [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    where valid (x, y) = (x >= 0) && (y >= 0)

main = do
    print $ length $ nub $ concat $ take 51 $ iterate (nub.allMoves) [(1, 1)]
        where allMoves x = concat $ map moves x
