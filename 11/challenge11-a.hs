import qualified Data.Map as M
import qualified Data.Set as Set

data Floor = Floor1 | Floor2 | Floor3 | Floor4 deriving (Ord, Eq, Show, Enum)
data Element = Polonium | Thulium | Promethium | Ruthenium | Cobalt deriving (Show, Eq, Ord)
data Item = Generator Element | Microchip Element deriving (Show, Eq, Ord)
data State = State (M.Map Floor (Set.Set Item)) Floor deriving (Show, Eq)
data StateEquiv = StateEquiv (M.Map Floor [Floor]) Floor deriving (Show, Eq)

startState = State (M.fromList [(Floor1, Set.fromList [Generator Polonium, Generator Thulium, Microchip Thulium, Generator Promethium, Generator Ruthenium, Microchip Ruthenium, Generator Cobalt, Microchip Cobalt]), (Floor2, Set.fromList [Microchip Polonium, Microchip Promethium]), (Floor3, Set.empty), (Floor4, Set.empty)]) Floor1
endState = State (M.fromList [(Floor1, Set.empty), (Floor2, Set.empty), (Floor3, Set.empty), (Floor4, Set.fromList [Generator Polonium, Microchip Polonium, Generator Thulium, Microchip Thulium, Generator Promethium, Microchip Promethium, Generator Ruthenium, Microchip Ruthenium, Generator Cobalt, Microchip Cobalt])]) Floor4

--startState = State (M.fromList [(Floor1, Set.fromList [Microchip Polonium, Microchip Thulium]), (Floor2, Set.fromList [Generator Polonium]), (Floor3, Set.fromList [Generator Thulium]), (Floor4, Set.empty)]) Floor1
--endState = State (M.fromList [(Floor1, Set.empty), (Floor2, Set.empty), (Floor3, Set.empty), (Floor4, Set.fromList [Microchip Polonium, Microchip Thulium, Generator Polonium, Generator Thulium])]) Floor4

paired item items = case item of
    Generator a -> Set.member (Microchip a) items
    Microchip a -> Set.member (Generator a) items

pairedGen items = 0 == (Set.size $ Set.filter isGen items)
    where isGen x = case x of
            Generator _ -> True
            _ -> False

pairedChip items = foldl (\x y -> paired y items && x) True $ Set.filter isChip items
    where isChip x = case x of
            Microchip _ -> True
            _ -> False

valid (State floors _) = (pairedGen (floors M.! Floor1) || pairedChip (floors M.! Floor1)) &&
                            (pairedGen (floors M.! Floor2) || pairedChip (floors M.! Floor2)) &&
                            (pairedGen (floors M.! Floor3) || pairedChip (floors M.! Floor3)) &&
                            (pairedGen (floors M.! Floor4) || pairedChip (floors M.! Floor4))

makeEquiv :: State -> StateEquiv
makeEquiv (State x floor) = StateEquiv (M.map equivSet x) floor
    where fromItem y = case y of
            Generator a -> fst $ head $ M.toList $ M.filter (Set.member $ Microchip a) x
            Microchip a -> fst $ head $ M.toList $ M.filter (Set.member $ Generator a) x
          equivSet z = map fromItem $ Set.toList z

moves :: State -> [State]
moves (State floors current) = filter valid $
    case current of
        Floor1 -> moveto Floor2
        Floor4 -> moveto Floor3
        _ -> (moveto $ succ current)++(moveto $ pred current)
        where moveto floor = [State (M.adjust (Set.delete x) current (M.adjust (Set.insert x) floor floors)) floor | x <- Set.toList (floors M.! current)]++
                                [State (M.adjust ((Set.delete x).(Set.delete y)) current (M.adjust ((Set.insert y).(Set.insert x)) floor floors)) floor | x <- Set.toList (floors M.! current), y <- Set.toList (floors M.! current), x<y]

breadth current end n visited (y:ys) (x:xs) = if elem end next then n+1 else breadth x end y (visited++(map makeEquiv next)) (ys++(replicate (length next) (n+1))) (xs++next) where next = filter (\x -> not (elem (makeEquiv x) visited)) $ moves current
breadth current end n visited [] [] = if elem end next then n+1 else breadth (head next) end (head ((replicate (length next) (n+1)))) (visited++(map makeEquiv next)) ((replicate (length next) (n+1))) (next) where next = filter (\x -> not (elem (makeEquiv x) visited)) $ moves current

main = do
    print $ breadth startState endState 0 [] [] []
