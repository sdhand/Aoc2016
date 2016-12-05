import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack)

password id index = if take 5 md5hash == "00000" then [md5hash !! 5]++(password id (index+1)) else password id (index+1)
    where md5hash = show $ md5 $ pack $ id++(show index)

main = do
    putStrLn $ take 8 $ password "abbhdwsy" 0
