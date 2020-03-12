isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:x1:xs) = if x <= x1 && isSorted (x1:xs) then True else False
    
    