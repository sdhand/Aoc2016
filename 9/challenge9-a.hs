decompress [] = []
decompress (x:xs) = case x of
    '(' -> (concat $ replicate count $ take n rest)++(decompress $ drop n rest)
    '\n' -> decompress xs
    _ -> x:(decompress xs)
    where (a, (_:bs))=span ('x' /=) xs
          (c, (_:rest))=span (')' /=) bs
          n=read a
          count=read c

main = do
    input <- readFile "input.txt"
    print $ length $ decompress input
