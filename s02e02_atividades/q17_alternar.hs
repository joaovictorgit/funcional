alternar' [] = []
alternar' (x:xs) = x:(x *(-1)): alternar' xs