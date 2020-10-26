-- Q1
data Expr = Val Int
    | Soma Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | Sub Expr Expr
    |Mod Expr Expr
    deriving (Read,Eq,Show)


eval :: Expr -> Int
eval (Val x) = x
eval (Soma exp1 exp2) = eval exp1 + eval exp2
eval (Mult exp1 exp2) = eval exp1 * eval exp2
eval (Div exp1 exp2) = div (eval exp1) (eval exp2)
eval (Sub exp1 exp2) = eval exp1 - eval exp2
eval (Mod exp1 exp2) = mod (eval exp1) (eval exp2)

-- Mobile
data Mobile = Pendente Int | Barra Mobile Mobile deriving (Show)

peso :: Mobile -> Int
peso (Pendente x) = x
peso (Barra m1 m2) = peso m1 + peso m2


balanceado :: Mobile ->Bool
balanceado (Barra m1 m2) 
    | peso m1 == peso m2 = True
    | otherwise = False




    