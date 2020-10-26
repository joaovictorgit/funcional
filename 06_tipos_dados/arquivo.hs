import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Numeric ( showFFloat )

-- Q1
data Complex = Complex { real :: Float ,img :: Float}

instance Show Complex where
    show (Complex a b) =  showFFloat (Just 1) a  " " ++ " + " ++ showFFloat (Just 1) b "i " 

instance Num Complex where
    (+) (Complex q0 q1) (Complex q2 q3) = (Complex (q0+q2) (q1+q3))
    (-) (Complex q0 q1) (Complex q2 q3) = (Complex (q0-q2) (q1-q3))
    (*) (Complex q0 q1) (Complex q2 q3) = (Complex ((q0*q2)-(q1*q3)) ((q0*q3)+(q1*q2)))
    negate (Complex q0 q1) = (Complex (negate q0) (negate q1))
    abs (Complex q0 q1) =  (Complex (abs (sqrt (q0^2 + q1^2))) 0)
    signum (Complex q0 q1)   = (Complex (sqrt q0 - signum q0) (sqrt q1 - signum q1))
    fromInteger n = (Complex (fromIntegral n :: Float) 0)

-- Q2
data Mes = Janeiro 
            | Fevereiro 
            | Marco 
            | Abril 
            | Maio 
            | Junho 
            | Julho 
            | Agosto 
            | Setembro 
            | Outubro
            | Novembro
            | Dezembro
            deriving (Show, Ord, Eq, Enum) 

data Date = MkDate { dia :: Int 
            , mes :: Mes    
            , ano :: Int   
            } 

instance Show Date where
    show (MkDate a b c)= show a ++ " de " ++ show b ++ " de " ++ show c

instance Eq Date where
    (==) (MkDate d0 m0 a0) (MkDate d1 m1 a1) = if (d0==d1 && m0==m1 && a0==a1) then True else False
    

instance Ord Date where
   (<=) (MkDate d0 m0 a0) (MkDate d1 m1 a1) 
    | a0-a1 >= 0 = False
    | otherwise = True

    

-- Q3
type Row = [Float]
data Matrix = Matrix { ncols :: Int
                    ,  nrows :: Int 
                    ,  rows :: [Row]
                    }

-- matriz de zeros
--zeroMatrix :: Int -> Int -> Matrix
--zeroMatrix a b = Matrix a b 
    
    
-- matriz de uns
--oneMatrix :: Int -> Int -> Matrix
--oneMatrix a b = Matrix a b 
-- matriz identidade : recebe ordem
--identMatrix :: Int -> Matrix
-- soma duas matrizes
--sumMatrix :: Matrix -> Matrix -> Matrix
-- produto de escalar por matriz
--prodScalar :: Float -> Matrix -> Matrix
-- produto entre matrizes
--prodMatrix :: Matrix -> Matrix -> Matrix
-- transforma listas de listas de
-- floats numa matriz
--listToMatrix :: [Row] -> Matrix

instance Show Matrix where
    show = undefined
    --show (Matrix a b [c]) =  "|" ++ [x | (x,y) <- zip (Matrix a b [c])] ++ [y | (x,y) <- zip (Matrix a b )] ++ " |"
    
    
-- Q5    pee
data Pessoa = Pessoa { nome :: String , idade :: Int, salario :: Float } deriving(Eq)

data Criterio = ByNome | ByIdade | BySalario deriving(Eq)

-- classifica lista de pessoa por critério
sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa pessoas cri
    | cri == ByNome = sortBy (compare `on`  nome) pessoas
    | cri == ByIdade = sortBy (compare `on`  idade) pessoas
    | otherwise = sortBy (compare `on`  salario) pessoas


instance Show Pessoa where
    show (Pessoa n i s) = show n ++ " tem " ++ show i ++ " anos e ganha um salario " ++ show s
pessoas = [ Pessoa "Joao" 25 2000, Pessoa "Ana" 20 2500, Pessoa "Alyson" 22 2200]