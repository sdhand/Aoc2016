import Crypto.Hash (hashlazy, MD5, Digest)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (partition)
import Data.Maybe

data State = State (Int, Int) String deriving (Show, Eq)

hash :: String -> String
hash route = show $ ((hashlazy $ pack ("qtetzkpl"++route)) :: Digest MD5)

open :: Char -> Bool
open c = c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f'

moves :: State -> [State]
moves (State (x, y) route) =
    (if vu then [State up (route++"U")] else [])++
    (if vd then [State down (route++"D")] else [])++
    (if vl then [State left (route++"L")] else [])++
    (if vr then [State right (route++"R")] else [])
      where up = (x,y-1)
            down = (x,y+1)
            left = (x-1, y)
            right = (x+1, y)
            valid (x', y') = x' >= 0 && y' >= 0 && x' <= 3 && y' <= 3
            (vu:vd:vl:vr:[]) = zipWith (&&) (map open $ take 4 $ hash route) (map valid [up,down,left,right])

longest :: [State] -> (Maybe State) -> State
longest (x:xs) sofar = if (length y) > 0 then longest (xs++next) (Just $ head y) else longest (xs++next) sofar
    where (y, next) = partition (\(State y _) -> y == (3, 3)) $ moves x
longest [] (Just sofar) = sofar

main = do
    print $ length route
        where (State _ route) = longest [State (0,0) ""] Nothing
