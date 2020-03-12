rotDir :: Int -> String -> String

contador a = a + 1
tamanho nome = sum[contador 0 | a <- nome]


rotDir n nome 
    | n >= 2 = reverse (take (n `mod` length nome)  (reverse nome)) ++ reverse (drop (n `mod` length nome) (reverse nome))
    | otherwise =  take (n) (reverse nome) ++ reverse (drop n (reverse nome))