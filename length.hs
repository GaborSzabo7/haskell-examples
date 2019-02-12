-- length.hs, contains all version of length function
length' :: (Num a) => [a] -> a
length'     [] = 0
length'    [x] = 1
length' [_:xs] = 1 + length' xs

length'' :: (Num a) => [a] -> a
length'' x = sum [1 | _ <- x]
