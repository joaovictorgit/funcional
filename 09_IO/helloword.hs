

n_vezes 1 a = a
n_vezes n a = a ++ " "++ n_vezes (n-1) a

main = do
    letras <- getLine
    let x = read letras :: Int
    putStrLn $ (n_vezes x "Ola Mundo")