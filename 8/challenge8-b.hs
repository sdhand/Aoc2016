import Data.List
rect x y screen = map ((replicate x True ++).drop x) (take y screen) ++ drop y screen

rotateR y n screen = a++((drop rotate row)++(take rotate row)):xs
    where row = screen !! y
          rotate = (length row)-n
          (a, _:xs) = splitAt y screen

rotateC x n screen = transpose $ rotateR x n $ transpose screen

screen = replicate 6 (replicate 50 False)

parse screen line = case head $ commands of
    "rect" -> rect (read a) (read $ tail b) screen where (a, b) = span ((/=) 'x') (last commands)
    "rotate" -> case commands !! 1 of
        "row" -> rotateR (read (drop 2 (commands !! 2))) (read (commands !! 4)) screen
        "column" -> rotateC (read (drop 2 (commands !! 2))) (read (commands !! 4)) screen
   where commands = words line

main = do
    input <- readFile "input.txt"
    putStrLn $ concat $ intersperse "\n" $ map (map (\x -> if x then '█' else ' ')) $ foldl parse screen $ lines input
