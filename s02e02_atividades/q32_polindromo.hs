polindromo nome = if nome == reverse nome then True else False

main = do
    nome <- getLine
    print $ polindromo nome