abba (w:x:y:z:[]) = (w /= x) && (w == z) && (x == y)
abba x = False

supportsTLS hypernet valid (w:x:y:z:xs) = not (abba (w:x:y:[z]) && hypernet) && supportsTLS (x=='[' || (hypernet && not(x==']'))) (abba (w:x:y:[z]) || valid) (x:y:z:xs)
supportsTLS _ valid x = valid

main = do
    input <- readFile "input.txt"
    print $ length $ filter (supportsTLS False False) $ lines input
