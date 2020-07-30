import Data.Char
import Data.List

-- 01 menorDeDois
menorDeDois a b = if a < b then a else b

-- 02 menorDeTres
menorDeTres a b c = if a < b && a < c then a else if b < a && b < c then b else c

-- 03 fatorial
fat 0 = 1
fat 1 = 1
fat n = n * fat(n-1)

-- 04 fibomacci
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

-- 05 elemento
elemento n xs = xs !! n

-- 06 pertence
pertence n [] = False
pertence 0 _ = False
pertence n (x:xs) = if n == x then True else pertence n xs

-- 07 total
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- 08 maior
maior [x] = x
maior (x:xs:xss) = if x > maior (xs:xss) then x else maior (xs:xss)

-- 09 frequencia
frequencia n xs = sum [1 | a <- xs, a == n] 

-- 10 unico 
unico n xs  
    | (valor >= 2) = False
    | (valor == 0) = False
    | otherwise = True
    where valor = sum (filter (== n) [a | a <- xs])

-- 11 maioresQue
maioresQue n xs = [a | a <- xs, a > n]

-- 12 concat
concat' a b = [x | x <- a] ++ [y | y <- b]

-- 13 calda
calda (x:xs) = xs

-- 14 corpo
corpo [x] = []
corpo (x:xs) = x: corpo xs 

-- 15 unique
unique' [] = []
unique' [x] = [x]
unique' (x:xs) = x:(unique' $ filter (/=x) xs)

-- 16 menores
menores' 0 xs selecionados aux = aux
menores' n [] selecionados aux = aux
menores' n (x:xs) selecionados aux = if x `elem` selecionados then menores' (n-1) xs selecionados (aux++[x]) else menores' n xs selecionados aux

menores n xs = menores' n xs (take n (sort xs)) []


-- 17 alter
alter' [] = []
alter' (x:xs) = x:(-1)*x: alter' xs
   
-- 18 reverso
reverso xs = reverse xs

-- 19 divide
divide _ [] = ([], [])
divide n (x:xs) = 
    if n == 0 
        then ([], x:xs) 
        else (x:x1, x2)
    where (x1,x2) = divide (n-1) xs

-- 20 itercal
intercal [] [] = []
intercal [] (y:ys) = (y:ys)
intercal (x:xs) [] = (x:xs)
intercal (x:xs) (y:ys) = x:y: intercal xs ys


-- 21 uniao
uniao [] [] =[]
uniao [] (y:ys) = (y:ys)
uniao (x:xs) [] =(x:xs)
uniao (x:xs)(y:ys)
    | x < y = x:uniao xs (y:ys)
    | x==y = x:uniao xs ys
    | otherwise = y: uniao (x:xs) ys

-- 22 intersec   [3,6,5,7] [9,7,5,1,3]


-- 23 sequencia
sequencia n m 
    | (n == 0) = []
    | otherwise = [a | a <- [m..(n+m-1)]]
    
-- 24 inserir
inserir n (x:xs)
    | (isSorted (x:xs) == True) && (n < x)= n:(x:xs)
    | (isSorted (x:xs) == True) && (n > x) = quicksort (n:(x:xs))
    | otherwise = (x:xs)
    
-- 25 isSorted
isSorted [] = True
isSorted [x] = True
isSorted (x:xs:xss)
    | x <= xs && isSorted (xs:xss) = True
    | otherwise = False


-- 26 quicksort
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = 
    let menor_x = quicksort [a | a <- (x:xs), a < x]
        maior_x = quicksort [a | a <- (x:xs), a > x]
    in menor_x ++ [x] ++ maior_x


-- 27 rotEsq    rotEsq 1 "asdfg" ==> "sdfga"
rotEsq :: Int -> String -> String
rotEsq n xs 
    | (n == 0) = xs
    | otherwise = (drop (mod n (length xs)) xs) ++ (take  (mod n (length xs)) xs)


-- 28 rotDir    rotDir 1 "asdfg" ==> "gasdf"
rotDir :: Int -> String -> String 
rotDir n xs
    | (n == 0) = xs
    | otherwise = (take (mod n (length xs)) (reverse xs)) ++ reverse (drop (mod n (length xs)) (reverse xs))

-- 29 upper
l_int [] = []
l_int (x:xs) 
    | ((ord x >= 97) && (ord x < 123)) = (ord x)-32: l_int xs
    | otherwise = (ord x) : l_int xs

upper xs = [chr x | x <- (l_int xs)]

   
-- 30 titulo

-- 31 selec "abcdef"[0,3,2,3] ==> "adcd"
selec _ [] = []
selec nome (x:xs) = nome !! x: selec nome xs


-- 32 isPalind
isPalind xs 
    | (xs /= reverse xs) = False
    | otherwise = True

-- 33 primo
primo :: Int -> Bool
primo n = if ([a | a <- [1..n], mod n a == 0]) == [1,n] then True else False


-- 34 sdig  328464584658 ==> 63
sdig 0 = 0
sdig n = (mod n 10) + (sdig (div n 10))

-- 35 bubblesort
bubblesort [] = []
bubblesort xs = bubble_ordena xs (length xs)

bubble_ordena xs 0 = xs
bubble_ordena xs n = bubble_ordena (troca xs) (n-1)

troca [x] = [x]
troca (x:x1:xs)
    | x > x1 = x1: troca (x:xs)
    | otherwise = x: troca (x1:xs)


-- 37 splitints
par xs = [x | x <- xs, mod x 2 == 0]
impar xs = [x | x <- xs, mod x 2 == 1]    
splitints xs = ((impar xs), (par xs))

-- 38 quad_perfeito
--raiz :: Int -> Int
quad_perfeito n 
    | sum [x | x <- [1..(div n 2)], ((x*x)==n)] > 0 = True
    | otherwise = False