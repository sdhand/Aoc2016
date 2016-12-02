import qualified Data.Map as Map

keypad1 = [ [Just '1', Just '2', Just '3'],
            [Just '4', Just '5', Just '6'],
            [Just '7', Just '8', Just '9'] ]

keypad2 = [ [Nothing,  Nothing,  Just '1', Nothing,  Nothing ],
            [Nothing,  Just '2', Just '3', Just '4', Nothing ],
            [Just '5', Just '6', Just '7', Just '8', Just '9'],
            [Nothing,  Just 'A', Just 'B', Just 'C', Nothing ],
            [Nothing,  Nothing,  Just 'D', Nothing,  Nothing ] ]

rowMap [] _ _ y = y
rowMap (x:xs) row column y = rowMap xs row (column+1) $ Map.insert (column, row) x y

buildKeypadMap [] _ y = y
buildKeypadMap (x:xs) row y = buildKeypadMap xs (row-1) $ rowMap x row 0 y

keypadMap2 = buildKeypadMap keypad2 0 Map.empty
keypadMap1 = buildKeypadMap keypad1 0 Map.empty

move (a, b) 'U' = (a, b+1)
move (a, b) 'D' = (a, b-1)
move (a, b) 'R' = (a+1, b)
move (a, b) 'L' = (a-1, b)

code keypadMap (x,y) z = case nextCode of
        (_, Nothing) -> (x, y)
        (a, Just b) -> (a, b)
    where result = move x z
          nextCode = if Map.notMember result keypadMap then (x, keypadMap Map.! x) else (result, keypadMap Map.! result)

solveKeypad :: Map.Map (Int, Int) (Maybe Char) -> String -> ((Int, Int), Char) -> String
solveKeypad keypad input start = map (snd.(foldl (code keypad) start)) (lines input)

main = do
    input <- readFile "input.txt"
    putStrLn $ solveKeypad keypadMap2 input ((0, -2), '5')
    putStrLn $ solveKeypad keypadMap1 input ((1, -1), '5')
