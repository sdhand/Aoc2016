import Data.List
data FacingDirection = North | East | South | West deriving Enum

data TurnDirection = R | L deriving Read

turn :: TurnDirection -> FacingDirection -> FacingDirection
turn R West = North
turn L North = West
turn R x = succ x
turn L x = pred x

move :: FacingDirection -> Int -> (Int, Int) -> [(Int, Int)]
move North x (a, b) = reverse $ map (\t -> (a, t)) [b+1..b+x]
move East  x (a, b) = reverse $ map (\t -> (t, b)) [a+1..a+x]
move South x (a, b) = map (\t -> (a, t)) [b-x..b-1]
move West  x (a, b) = map (\t -> (t, b)) [a-x..a-1]

repeatedLocation :: [String] -> [(Int, Int)] -> FacingDirection -> (Int, Int)
repeatedLocation ((x:ys):zs) previousLocations facing = case intersect nextLocations previousLocations of
    a:as -> a
    [] ->  repeatedLocation zs (nextLocations++previousLocations) nextFacing
    where nextFacing = (turn (read $ [x]) facing)
          nextLocations = (move nextFacing (read ys) $ head previousLocations)

removeComma s = if last s == ',' then init s else s

main = do
    input <- readFile "input.txt"
    let location = repeatedLocation (map removeComma $ words input) [(0,0)] North in print $ (abs $ fst location) + (abs $ snd location)
