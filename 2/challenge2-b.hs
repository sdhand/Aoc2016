import qualified Data.Map as Map

keypad = [ [Nothing,  Nothing,  Just '1', Nothing,  Nothing ],
           [Nothing,  Just '2', Just '3', Just '4', Nothing ],
           [Just '5', Just '6', Just '7', Just '8', Just '9'],
           [Nothing,  Just 'A', Just 'B', Just 'C', Nothing ],
           [Nothing,  Nothing,  Just 'D', Nothing,  Nothing ] ]

rowMap [] _ _ y = y
rowMap (x:xs) row column y = rowMap xs row (column+1) $ Map.insert (column, row) x y

buildKeypadMap [] _ y = y
buildKeypadMap (x:xs) row y = buildKeypadMap xs (row-1) $ rowMap x row 0 y

keypadMap = buildKeypadMap keypad 0 Map.empty

move (a, b) 'U' = (a, b+1)
move (a, b) 'D' = (a, b-1)
move (a, b) 'R' = (a+1, b)
move (a, b) 'L' = (a-1, b)

code :: ((Int, Int), Char) -> Char -> ((Int, Int), Char)
code (x,y) z = case nextCode of
        (_, Nothing) -> (x, y)
        (a, Just b) -> (a, b)
    where result = move x z
          nextCode = if Map.notMember result keypadMap then (x, keypadMap Map.! x) else (result, keypadMap Map.! result)

main = do
    input <- readFile "input.txt"
    putStrLn $ map (snd.(foldl code ((0, -2), '5'))) (lines input)
