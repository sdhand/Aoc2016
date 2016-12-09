decompressLength [] = 0
decompressLength (x:xs) = case x of
    '(' -> (count*(decompressLength $ take n rest))+(decompressLength $ drop n rest)
    '\n' -> decompressLength xs
    _ -> 1+decompressLength xs
    where (a, (_:bs))=span ('x' /=) xs
          (c, (_:rest))=span (')' /=) bs
          n=read a
          count=read c

main = do
    input <- readFile "input.txt"
    print $ decompressLength input
