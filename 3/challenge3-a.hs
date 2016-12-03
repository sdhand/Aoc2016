valid (x:y:z:[]) = if x+y>z && x+z>y && y+z>x then True else False

count possible [] = possible
count possible (x:xs) = if valid x then count (possible+1) xs else count possible xs

main = do
    input <- readFile "input.txt"
    print $ count 0 $ map ((map read).words) (lines input)
