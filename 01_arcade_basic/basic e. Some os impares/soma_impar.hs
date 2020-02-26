lista [] = 0
soma_impar lista = sum[x | x <- lista, mod x 2 == 1]
