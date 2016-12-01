data FacingDirection = North | East | South | West deriving Enum

data TurnDirection = R | L deriving Read

turn :: TurnDirection -> FacingDirection -> FacingDirection
turn R West = North
turn L North = West
turn R x = succ x
turn L x = pred x

move :: FacingDirection -> Int -> (Int, Int) -> (Int, Int)
move North x (a, b) = (a, b+x)
move East  x (a, b) = (a+x, b)
move South x (a, b) = (a, b-x)
move West  x (a, b) = (a-x, b)

finalLocation :: [String] -> (Int, Int) -> FacingDirection -> (Int, Int)
finalLocation [] location _ = location
finalLocation (x:xs) location facing = finalLocation xs (move nextFacing ((read :: String -> Int) $ tail x) location) nextFacing where nextFacing = (turn ((read :: String -> TurnDirection) $ [head x]) facing)

removeComma s = if last s == ',' then init s else s

main = do
    input <- readFile "input.txt"
    let location = finalLocation (map removeComma $ words input) (0,0) North in print $ (abs $ fst location) + (abs $ snd location)
