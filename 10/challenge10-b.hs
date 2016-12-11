import qualified Data.Map as M
import Data.List

data Output = Bot Int | Tray Int
data Bot = B [Int] (Output, Output)

full (B x _) = length x == 2

give bot n bots = M.insert bot (B (n:x) y) bots
    where (B x y) = bots M.! bot

update (bots, output) x min max low high = case low of
    Bot z -> (give z min highbot, highout)
    Tray z -> (highbot, M.insert z min highout)
    where clearbot = M.insert x (B [] (low, high)) bots
          (highbot, highout) = case high of
            Bot y -> (give y max clearbot, output)
            Tray y -> (clearbot, M.insert y max output)

prodOut (Just a) (Just b) (Just c) = Right (a*b*c)
prodOut _ _ _ = Left Nothing

runBots (bots, output) = case prodOut (M.lookup 0 output) (M.lookup 1 output) (M.lookup 2 output) of
    Right a -> a
    _ -> runBots $ update (bots, output) bot min max low high
        where (bot, B x (low, high)) = head $ M.toList $ M.filter full bots
              min = minimum x
              max = maximum x

buildBot :: [String] -> M.Map Int Bot -> [String] -> M.Map Int Bot
buildBot b bots command = M.insert (read $ command !! 1) (B (values $ read $ command !! 1) (output (command !! 5) (read $ command !! 6), output (command !! 10) (read $ command !! 11))) bots
    where output :: String -> Int -> Output
          output place n = if place == "bot" then Bot n else Tray n
          values :: Int -> [Int]
          values n =  map (read.(!! 1)) $ filter ((==(show n)).(!! 5)) $ map words b

findBot :: String -> Int
findBot input = runBots (foldl (buildBot b) M.empty $ map words a, M.empty)
    where (a, b) = span (isPrefixOf "bot") $ sort $ lines input

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ findBot input
