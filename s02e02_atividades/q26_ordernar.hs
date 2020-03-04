ordenar :: (Ord a) => [a] -> [a]  
ordenar [] = []  
ordenar (x:xs) = 
    let menor_x = ordenar [a | a <- xs, a <= x]  
        maior_x = ordenar [a | a <- xs, a > x]  
    in  menor_x ++ [x] ++ maior_x