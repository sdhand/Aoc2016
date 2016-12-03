import Data.List

valid (x:y:z:[]) = x+y>z && x+z>y && y+z>x

column3 (x:y:z:xs) = transpose [x,y,z] ++ column3 xs
column3 x = []

main = do
    input <- readFile "input.txt"
    print $ length $ filter valid $ map (map read.words) $ lines input
    print $ length $ filter valid $ column3 $ map (map read.words) $ lines input
