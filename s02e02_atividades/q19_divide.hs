dividir :: Int -> [a] -> ([a], [a])
dividir n [] = ([], [])
dividir n (x:xs) = 
    if n == 0 
        then ([], x:xs) 
        else (x:xs1, xs2)
    where (xs1,xs2) = dividir (n - 1) xs
            
