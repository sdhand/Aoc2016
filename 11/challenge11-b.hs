import Data.List
import Debug.Trace

data Floor = Floor1 | Floor2 | Floor3 | Floor4 deriving (Eq, Ord, Enum, Show)
data State = State [(Floor, Floor)] Floor deriving (Eq, Ord, Show)

valid :: State -> Bool
valid (State items _) = and $ map validFloor [Floor1, Floor2, Floor3, Floor4]
    where filterFloor floor func = filter (\x -> (func x)==floor) items
          validFloor floor = (all (\(x, y) -> x==y) $ filterFloor floor snd) || ((length $ filterFloor floor fst) == 0)

startState = State (sort  [(Floor1, Floor1), (Floor1, Floor1), (Floor1, Floor1), (Floor1, Floor1), (Floor1, Floor1), (Floor1, Floor2), (Floor1, Floor2)]) Floor1
endState =   State (sort [(Floor4, Floor4), (Floor4, Floor4), (Floor4, Floor4), (Floor4, Floor4), (Floor4, Floor4), (Floor4, Floor4), (Floor4, Floor4)]) Floor4

move1 :: State -> Floor -> [State]
move1 (State items current) next = nub $
    [State (sort $ (next, y):(delete (x, y) items)) next | (x, y) <- items, x==current]++
    [State (sort $ (x, next):(delete (x, y) items)) next | (x, y) <- items, y==current]

move2 :: State -> Floor -> [State]
move2 (State items current) next = nub $ concat $ map (`move1` next) moved
    where moved = map (\(State x _) -> State x current) $ move1 (State items current) next

moves :: State -> [State]
moves (State items current) = case current of
    Floor1 -> up
    Floor2 -> if empty Floor1 then up else up++down
    Floor3 -> if (empty Floor2) && (empty Floor1) then up else up++down
    Floor4 -> down
    where empty floor = (length $ filter (\(x, y) -> (x == floor) || (y == floor)) items) == 0
          up1 = filter valid $ move1 (State items current) $ succ current
          up2 = filter valid $ move2 (State items current) $ succ current
          up = if (length up2) == 0 then up1 else up2
          down1 = filter valid $ move1 (State items current) $ pred current
          down2 = filter valid $ move2 (State items current) $ pred current
          down = if (length down1) == 0 then down2 else down1

shortest :: [(Int, State)] -> State -> [State] -> Int
shortest ((depth, current):xs) end visited = if any (== end) next then depth + 1 else shortest ((map (\x -> (depth+1, x)) next)++xs) end (visited++next)
    where next = nub $ filter (not.(`elem` visited)) $ moves current

main :: IO ()
main = do
    print $ shortest [(0, startState)] endState [startState]
