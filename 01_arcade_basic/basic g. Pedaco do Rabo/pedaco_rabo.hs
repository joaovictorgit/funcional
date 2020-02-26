lista [] = 0

func_last' lista indice =  reverse(take indice(reverse lista))

func_lista lista indice = if indice == 1 then func_last' lista indice else if length lista > indice then drop indice [x | x <- lista] else [x | x <- lista]