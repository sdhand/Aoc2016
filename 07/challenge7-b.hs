import Data.List

blocks hypernetBlock (a, b) (x:xs) = case x of
    '[' -> blocks True (if b==[] then (a++[' '], b) else (a++[' '], b++[' '])) xs
    ']' -> blocks False (a, b) xs
    _   -> if hypernetBlock then blocks True (a, b++[x]) xs else blocks False (a++[x], b) xs
blocks _ x [] = x

supportsSSL ((x:y:z:xs), b) = ((x /= y) && (x==z) && (x /= ' ') && (y /= ' ') && (isInfixOf (y:x:[y]) b)) || supportsSSL ((y:z:xs), b)
supportsSSL (a, b) = False

main = do
    input <- readFile "input.txt"
    print $ length $ filter (supportsSSL.(blocks False ([], []))) $ lines input
