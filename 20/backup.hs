import Data.List
import Debug.Trace

buildFunc func range = \x -> traceShow x $ (func x) && (outRange x)
    where (a,(_:b)) = span (\x -> x /= '-') range
          outRange x = (x>(read b)) || (x<(read a))

main = do
    input <- readFile "input.txt"
    print $ head $ filter (\(x, y) -> x) $ zip (map (func input) nums) nums
        where nums = [0..]
              func input = foldl buildFunc (const True) $ lines input
