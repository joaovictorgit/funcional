iguais3' x y z = 
    if x == y && y == z && x == z
      then 3
      else if x == y || y == z || x == z
        then 2
      else 0


main = do
  line1 <- getLine;
  line2 <- getLine;
  line3 <- getLine;

  let x = read line1 :: Int
  let y = read line2 :: Int
  let z = read line3 :: Int
  

  print $ iguais3' x y z