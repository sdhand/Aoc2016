import Data.ByteString.Lazy.Char8 (pack)
import Crypto.Hash (hashlazy, MD5, Digest)
import Data.List
import Data.Maybe

md5hash :: String -> String
md5hash x = show $ ((hashlazy $ pack x) :: Digest MD5)

hashes :: String -> Int -> [String]
hashes salt n = ((iterate md5hash (salt++(show n))) !! 2017):(hashes salt (n+1))

repeats n x
    | length x < n = Nothing
    | otherwise = if (length $ nub $ firstN) == 1 then Just $ head firstN else repeats n $ tail x
        where firstN = take n x

keys :: [(Int, String)] -> [(Int, String)]
keys (x:xs) = case repeats 3 $ snd x of
    Just a -> if any (\x -> isInfixOf (take 5 $ repeat a) $ snd x) $ take 1000 xs then x:(keys xs) else keys xs
    Nothing -> keys xs

main = do
    print $ (keys $ zip [0..] (hashes salt 0)) !! 63
        where salt="cuanljph"
