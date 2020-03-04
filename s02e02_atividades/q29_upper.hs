import Data.Char

maiscula :: [Char] -> [Char]
maiscula nome = [toUpper a | a <- nome] 
