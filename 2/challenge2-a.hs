code :: Int -> Char -> Int
code x 'U' = if result <= 0 then x else result where result = x-3
code x 'D' = if result >= 10 then x else result where result = x+3
code x 'R' = if (result-1) `div` 3 /= (x-1) `div` 3 then x else result where result = x+1
code x 'L' = if (result-1) `div` 3 /= (x-1) `div` 3 then x else result where result = x-1

main = do
    input <- readFile "input.txt"
    print $ map (foldl code 5) (lines input)
