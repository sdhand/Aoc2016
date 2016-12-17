import Crypto.Hash (hashlazy, MD5, Digest)
import Data.ByteString.Lazy.Char8 (pack)

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
            valid (x', y') = x' >= 0 && y' >= 0
            (vu:vd:vl:vr:[]) = zipWith (&&) (map open $ take 4 $ hash route) (map valid [up,down,left,right])

shortest :: [State] -> State
shortest (x:xs) = if (length ends) > 0 then head ends else shortest (xs++next)
    where next = moves x
          ends = filter (\(State y _) -> y == (3, 3)) next

main = do
    print $ shortest [State (0,0) ""]
