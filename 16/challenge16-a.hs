randData size seed= if length next >= size then next else randData size next
    where invert c = if c == '0' then '1' else '0'
          next = seed++"0"++(map invert $ reverse seed)

checksum (x:y:xs) = if x==y then '1':(checksum xs) else '0':(checksum xs)
checksum x = []

fullChecksum x = if odd $ length check then check else fullChecksum check
    where check = checksum x

main = do
    print $ fullChecksum $ take 35651584 $ randData 35651584 "10111100110001111"
