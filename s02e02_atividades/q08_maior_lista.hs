maximo' :: (Ord a) => [a] -> a
maximo' [] = error "Lista nula"
maximo' [x] = x
maximo' (x:xs)
    | x > max_tail = x
    | otherwise = max_tail
    where max_tail = maximo' xs


