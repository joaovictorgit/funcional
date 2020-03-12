select nome [] = nome
select nome (x:xs) = take (length (x:xs)) (nome !! x : select nome xs)