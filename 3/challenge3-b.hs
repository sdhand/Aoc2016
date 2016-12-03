valid (x,y,z) = if x+y>z && x+z>y && y+z>x then True else False

count possible [] = possible
count possible (x:xs) = if valid x then count (possible+1) xs else count possible xs

column3 (x:y:z:xs) = (zip3 x y z)++(column3 xs)
column3 x = []

main = do
    input <- readFile "input.txt"
    print $ count 0 $ column3 $ map ((map read).words) (lines input)
