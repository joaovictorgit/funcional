-- contatena
contatena xs ys = foldr (\ x y -> x:y) ys xs

-- inverte [1,2,,3,4] == [4,3,2,1]
inverte xs = foldr (\ x acc -> acc ++ [x]) [] xs

-- paridade fold
paridade xs = foldl (\ acc x -> acc == x) True xs


-- filtra filtraAplicaFold (4+) (<3) [1..7] == [5,6]
filtraAplicaFold xs = (foldl (\ acc x -> if x < 3 then acc++[x+4] else acc) [] xs)


-- mapFold mapFold (*2) [1,2,3] == [2,4,6]

mapFold xs =(foldr (\x succ -> x*2:succ) [] xs)


-- removeLista [1,2] [1,1,3,2,2,4,5] == [3,4,5]

removeLista xs ys = (filter (/= head xs)) (foldr (\ x z -> x:z) [] ys) 

--acertosFold "AEDBCCE" "ADDCCBE" == 4
acert_t xs ys = zip xs ys
acertosFold xs ys = foldr (\x acc -> if fst x == snd x then 1 + acc else 0+ acc) 0 (acert_t xs ys) 


-- descompactaFold [ (1,2),(3,4),(5,6),(4,5) ] == ( [ 1 , 3 , 5 , 4 ] , [ 2 , 4 , 6 , 5 ] ) 
descompactaFold xs = foldr (\ x acc -> (fst x : fst acc, snd x : snd acc)) ([],[]) xs 