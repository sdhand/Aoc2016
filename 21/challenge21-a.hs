import Data.Maybe
import Data.List
import Debug.Trace

swap :: Int -> Int -> String -> String
swap x' y' pass = first++[pass !! y]++second++[pass !! x]++last
    where x = min x' y'
          y = max x' y'
          (first, (_:next)) = splitAt x pass
          (second, (_:last)) = splitAt (y-x-1) next

replace :: Char -> Char -> String -> String
replace x y (x':xs)
    | x' == x = y:(replace x y xs)
    | x' == y = x:(replace x y xs)
    | otherwise = x':(replace x y xs)
replace _ _ [] = []

rotateR :: Int -> String -> String
rotateR x' pass = rotateL ((length pass)-x) pass
    where x = x' `mod` (length pass)

rotateL x' pass = let (a,b)=splitAt x pass in b++a
    where x = x' `mod` (length pass)

rotateB :: Char -> String -> String
rotateB x pass = rotateR (n+i+1) pass
    where Just i = elemIndex x pass
          n = if i >= 4 then 1 else 0

rev x y pass = first++(reverse second)++last
    where (first, next)=splitAt x pass
          (second, last)=splitAt (y-x+1) next

move :: Int -> Int -> String -> String
move x y pass = let (a,b) = splitAt y (delete (pass !! x) pass) in a++[pass !! x]++b

decode :: String -> [String] -> String
decode pass command
    | (command !! 0) == "swap" && (command !! 1) == "position" = swap (read $ command !! 2) (read $ command !! 5) pass
    | (command !! 0) == "swap" && (command !! 1) == "letter" = replace (head $ command !! 2) (head $ command !! 5) pass
    | (command !! 1) == "left" = rotateL (read $ command !! 2) pass
    | (command !! 1) == "right" = rotateR (read $ command !! 2) pass
    | (command !! 1) == "based" = rotateB (head $ command !! 6) pass
    | (command !! 0) == "reverse" = rev (read $ command !! 2) (read $ command !! 4) pass
    | (command !! 0) == "move" = move (read $ command !! 2) (read $ command !! 5) pass

main = do
    input <- readFile "input.txt"
    putStrLn $ foldl decode "abcdefgh" $ map words $ lines input
