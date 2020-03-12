inserir n xs = [a | a <- xs, n >= a]++ [n] ++ [a | a <- xs, n < a]
 