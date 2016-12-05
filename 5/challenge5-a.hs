import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack)
import Data.List

password id index = if isPrefixOf "00000" md5hash then [md5hash !! 5]++(password id (index+1)) else password id (index+1)
    where md5hash = show $ md5 $ pack $ id++(show index)

main = do
    putStrLn $ take 8 $ password "abbhdwsy" 0
