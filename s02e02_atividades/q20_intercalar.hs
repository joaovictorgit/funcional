intercal' [] [] = []
intercal' [] (z:zs) = z:zs
intercal' (x:xs) [] = x:xs
intercal' (x:xs) (z:zs) = x:z: intercal' (xs) (zs)