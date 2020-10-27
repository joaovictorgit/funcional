-- paridade  [True, True, False] => False
paridade xs = if mod (sum [1 | x <- xs, x == True]) 2 == 0 then False else True

-- rev 3491 ==> 1943 
rev x 
    | x < 0     = 0 - (read . reverse . tail . show $ x)
    | otherwise = read . reverse . show $ x


-- delete' 5 [1,5,6,9] ==> [1,6,9]
delete' n xs = (filter (/=n) [x | x <- xs])

-- swap [5,6,7,8,9] 0 3 ==> [8,6,7,5,9]


-- listacc [1,2,3,4] ==> [1,3,6,10]
soma [] = []
soma [x] = []
soma (x:x1:xs) = [x+x1]++ soma(x1:xs) 

listacc (x:xs) = x: (soma (x:xs)) ++ [sum [a | a <- (x:xs)]]



-- buscabin buscaBin [1,3,5,6,8] 5 ==> 2
buscabin xs n  
    | (elem n xs == True) = sum [1 | x <- xs, x <= n]
    | otherwise = -1


-- factors
primo n = if [x | x <- [1..n], mod n x == 0] == [1,n] then True else False
qtd_primos = filter (primo) [2..21]

contar (x:xs) n
    | mod n x /= 0 = 0:[]
    | otherwise = 1: contar [a | a <- xs] (div n x)

tam (x:xs) n = sum [k | k <- contar (x:xs) n]

factors n = tam