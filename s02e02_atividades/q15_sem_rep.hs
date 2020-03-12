sem_rep' :: (Eq a) => [a] -> [a]
sem_rep' []   = []
sem_rep' (x:[])  = [x]
sem_rep' (x1:x2:xs)
	| x1 == x2 = sem_rep' (x2:xs)
	| otherwise = x1: sem_rep'(x2:xs)
	

uniao [] = []
uniao (x:xs) = x:uniao (filter(/= x) xs)  

--[1,2,3,4,4,5,6]

