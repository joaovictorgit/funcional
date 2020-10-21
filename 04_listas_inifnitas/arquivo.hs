-- hammig
unir (x:xs) (y:ys) 
    | x < y = x : unir xs (y:ys)
    | x == y = x : unir xs ys
    | otherwise = y : unir (x:xs) ys

hamming = 1 : unir h2 (unir h3 h5)
   where h2 = map (2*) hamming
         h3 = map (3*) hamming
         h5 = map (5*) hamming
 

-- collatz 13 = [13,40,20,10,5,16,8,4,2,1]
verifica n 
    | n == 1 = 1
    | even n = div n 2 
    | otherwise = (3*n)+1

collatz n
    | n == 1 = [1]
    | otherwise = [n] ++ collatz (verifica n)


-- primos_gemeos
primo :: Int -> Bool
primo n = if ([a | a <- [1..n], mod n a == 0]) == [1,n] then True else False

todos_primos = [x | x <- [2..], primo x]

teste (x,y) = y - x == 2

primos_gemeos = filter teste (zip todos_primos (tail todos_primos))

-- goldbach
n_primos t = take t todos_primos

goldbach t = [(t,a,b) | a  <- (n_primos t), b <- (n_primos t), a+b == t] 


-- primosPalidromo
n_primos_10 = take 4 todos_primos
n_primos_99 = take 25 todos_primos
n_primos_100 = take 100 (drop 25 todos_primos)

primosPalindromo = n_primos_10 ++ list_palin_99 ++ list_palin_100
    where list_palin_100 = [b | b <- (n_primos_100), div b 100 == mod (b-100) 10]
          list_palin_99 = [a | a <- (n_primos_99), mod a 10 == div a 10]