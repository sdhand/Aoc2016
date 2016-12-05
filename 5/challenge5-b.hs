import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack)
import Data.List
import Data.Maybe

password id index = if isPrefixOf "00000" md5hash then [(read ("0x"++[md5hash !! 5]), md5hash !! 6)]++(password id (index+1)) else password id (index+1)
    where md5hash = show $ md5 $ pack $ id++(show index)

lookupAll n x = case lookup n x of
    Just a -> [a]++lookupAll (n+1) x
    Nothing -> []

main = do
    putStrLn $ take 8 $ lookupAll 0 $ password "abbhdwsy" 0
