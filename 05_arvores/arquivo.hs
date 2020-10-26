import Data.List (sortBy)
import Data.Function (on)

--extra
data ArvA a = NoA a (ArvA a) (ArvA a) | ArvVazia    deriving(Show, Eq)

mais_esq :: ArvA a -> a
mais_esq (NoA x ArvVazia dir) = x
mais_esq (NoA x esq dir) = mais_esq esq


mais_dir :: ArvA a -> a
mais_dir (NoA x esq ArvVazia) = x
mais_dir (NoA x esq dir) = mais_dir dir

remove :: Ord a => a -> ArvA a -> ArvA a
remove x (ArvVazia) = ArvVazia
remove x (NoA y ArvVazia ArvVazia) 
    | x == y = ArvVazia
    | x /= y = NoA y ArvVazia ArvVazia

remove x (NoA y esq ArvVazia)
    | x == y = NoA (maior) (remove maior esq) ArvVazia
    | x < y = NoA y (remove x esq) ArvVazia
    | x > y = NoA y esq ArvVazia
    where 
        maior = mais_dir esq

remove x (NoA y esq dir)
    | x == y = NoA (menor) esq (remove menor dir) 
    | x < y = NoA y (remove x esq) dir
    | x > y = NoA y esq (remove x dir)
    where 
        menor = mais_esq dir


-- Q1 insertArvore 2 Vazia == No 2 Vazia Vazia
data Arv a = Vazia | No a ( Arv a ) ( Arv a ) deriving (Eq , Show)

insertArvore :: Ord a => a -> Arv a -> Arv a
insertArvore x (Vazia) = No x (Vazia) (Vazia)
insertArvore x (No y esq dir)
    | x == y = (No y esq dir)
    | x < y = (No y (insertArvore x esq) dir)
    | x > y = (No y esq (insertArvore x dir))

-- Q2 
data Arvore a = Folha a | Ramo (Arvore a) (Arvore a) deriving (Show)
arv1 = Ramo (Folha 1) (Folha 2)

arv2 = Ramo (Folha 5) (Ramo (Folha 4) (Folha 3))

arv3 = Ramo (Ramo (Folha 5) (Folha 4)) (Ramo (Folha 2) (Ramo (Folha 1) (Folha 6)))


foldTree' (Folha x) = [x]
foldTree' (Ramo esq dir) = (foldTree' esq) ++ (foldTree' dir) 

foldTree  (Ramo esq dir) = foldr (\ x acc -> x + acc) 0 (foldTree' (Ramo esq dir))


-- Q3 makeMultiSet [9,3,1,1,4,2,1,7,8,9,2,1]  == MultiSet [(1,4),(2,2),(3,1),(4,1),(7,1),(8,1),(9,2)]
data MultiSet a = MultiSet [(a,Int)] deriving (Show)

cont [] = []
cont [x] = [x]
cont (x:x1:xs)
    | x /= x1 = cont (x1:xs)
    | otherwise = 1 : cont [k | k <- (x1:xs), x == x1]

qsort [] = []
qsort (x:xs) = 
    let menor_x = qsort [b | b <- xs, b <= x]
        maior_x = qsort [s | s <- xs, s > x]
    in menor_x ++ [x] ++ maior_x

makenumber (x:xs) = qsort (x:xs)

tam (x:xs) = length [c | c <- cont (x:xs)]

compac :: [Int] -> [[Int]]
compac [] = []
compac (x:xs) 
    | (tam (x:xs) <= 1) = comp1
    | otherwise = comp2
    where comp1 = [(x: [1])] ++compac (filter (/=x) [a | a <- (tail number)])
          comp2 = [x: [tam (x:xs)]] ++ compac (filter (/=x) [a | a <- (tail number)])
          number = makenumber (x:xs)

mySort (x:xs) = sortBy (compare `on` (\[a,b]->a)) t
    where t = compac (x:xs)
    
ordenar (x:xs) = mySort(x:xs)

makeMultiSet (x:xs) = MultiSet (tupla)
    where tupla = [(a,b) | [a,b] <- ordenar (x:xs)]


-- Q4 menorNivelFolha
data ArvBin a = VaziaBin | NoBin a (ArvBin a) (ArvBin a) deriving (Show)

menorNivel :: ArvBin a -> [a]
menorNivel (VaziaBin) = []
menorNivel (NoBin x (VaziaBin) (VaziaBin)) = [x]
menorNivel  (NoBin x esq dir) = (menorNivel esq) ++ (menorNivel dir) 

-- Q5 arvoreCheia
arvoreCheia (NoBin _ VaziaBin VaziaBin) = True
arvoreCheia (NoBin x esq dir)
    | esq == VaziaBin && dir /= VaziaBin = False
    | esq /= VaziaBin && dir == VaziaBin = False
    | otherwise = arvoreCheia esq 
