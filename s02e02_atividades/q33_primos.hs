fator n = [x | x <- [1..n], mod n x == 0]

primo n = if (fator n) == [1,n] then True else False