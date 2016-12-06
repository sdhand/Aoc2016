import Data.List
import Data.Ord
main = do
    input <- readFile "input.txt"
    putStrLn $ map (head . last . sortBy (comparing length) . group . sort) $ transpose $ lines input
