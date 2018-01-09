module Ficha3 where
import Data.List
import Data.Char


{-(x) :: Int -> Int -> Int
(x) 0 * n = 0 
(x) m*n = n + ((m-1)*n)-}

psdiv :: Int -> Int -> Int
psdiv n m | n > m = 0
          | otherwise = 1 + psdiv (m-n) n

psmod :: Int -> Int -> Int
psmod n m | n > m = m 
          | otherwise = psmod (m-n) n

pspower :: Int -> Int -> Int
pspower a 0 = 1
pspower a b = a + pspower a (b-1)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
conta :: Int -> Polinomio -> Int
conta 0 [] = 0
conta n ((a,b):t) = if n == b then 1 + conta n t else conta n t

--b)
grau :: Polinomio -> Int
grau [] = 0
grau [(a,b)] = b
grau ((a,b):t) = max b (grau t)

{-maximum :: Ord a => [a] -> a
maximum [h] = h
maximum (h:t) = max h (maximum t) -}

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((a,b):t) | x == b = (a,b) : selgrau x t
                    | otherwise = selgrau x t 

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) = ((a * fromIntegral b),(b-1)) : deriv t

--e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((a,b):t) = (x*a)^fromIntegral b + calcula x t 

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) = if b == 0 then simp t else (a,b) : simp t

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((x,y):t) = ((a*x),(b+y)) : mult (a,b) t 

--h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = aux h (normaliza t)
            where aux :: Monomio -> Polinomio -> Polinomio
                  aux (x,y) [] = [(x,y)]
                  aux (x,y) ((x1,y1):t) = if (y==y1) then ((x+x1),y) : t else (x1,y1) : (aux (x,y) t)

--i)
soma :: Polinomio -> Polinomio -> Polinomio
soma [] x = x
soma (h:t) p = soma t (soma_aux h p)
                where soma_aux :: Monomio -> Polinomio -> Polinomio
                      soma_aux x [] = []
                      soma_aux (x,y) ((x1,y1):t) = if (y==y1) then ((x+x1),y) : t else (x1,y1) : (soma_aux (x,y) t)

--j)
{-
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = produto_aux (normaliza p1) (normaliza p2)

produto_aux :: Polinomio -> Polinomio -> Polinomio
produto_aux [] x = []
produto_aux x [] = []
produto_aux ((a,b):t) ((a1,b1):t1) | b == b1 = (a*a1,b+b1) : produto t t1
                                   | otherwise = produto_aux (a,b) t1
-}
--type Polinomio = [Monomio]
--type Monomio = (Float,Int)
