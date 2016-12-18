data Tile = Safe | Trap deriving (Eq, Show)

makeTile :: Char -> Tile
makeTile '.' = Safe
makeTile '^' = Trap

nextRow :: Bool -> [Tile] -> [Tile]
nextRow first (x:y:z:xs) = (if x' /= z' then Trap else Safe):(nextRow False (y':z':xs'))
    where x' = if first then Safe else x
          y' = if first then x else y
          z' = if first then y else z
          xs' = if first then (z:xs) else xs
nextRow _ (x:_:[]) = [x]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ length $ filter (\x -> x == Safe) $ concat $ take 400000 $ iterate (nextRow True) $ map makeTile $ init input
