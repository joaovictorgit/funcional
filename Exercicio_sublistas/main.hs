--func lista a b = if b > a
--	then [x | x <- lista, x > a, x < b]
--	else [x | x <- lista, x > b, x < a]

  -- realizar subliesta positvos e negativos
lista [] = 0

tamanho lista = length lista  

func_miolo lista a b = if b < 0 then [x | x <- lista , x >= a, x <= tamanho lista + b] else [x | x <- lista, x >= a, x <= b]