module Ficha3 where
import Data.List
import Data.Char


(><) :: Int -> Int -> Int
(><) x y | y > 0 = x + (><) x (y-1)
         | otherwise = 0

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

conta2 :: Int -> Polinomio -> Int
conta2 x = length . filter (==x) . map snd

--b)
grau :: Polinomio -> Int
grau [] = 0
grau [(a,b)] = b
grau ((a,b):t) = max b (grau t)

grau2 :: Polinomio -> Int
grau2 = maximum . map snd

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((a,b):t) | x == b = (a,b) : selgrau x t
                    | otherwise = selgrau x t

selgrau2 :: Int -> Polinomio -> Polinomio
selgrau2 x = foldr (\l acc -> if x == snd l then l:acc else acc) []

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) = ((a * fromIntegral b),(b-1)) : deriv t

deriv2 :: Polinomio -> Polinomio
deriv2 = map (\(a,b) -> (a*fromIntegral b, pred b))

--e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((a,b):t) = (x*a)^fromIntegral b + calcula x t

calcula2 :: Float -> Polinomio -> Float
calcula2 x = foldr (\(a,b) acc -> acc + (x*a)^fromIntegral b) 0

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) = if b == 0 then simp t else (a,b) : simp t

simp2 :: Polinomio -> Polinomio
simp2 = filter (\(a,b) -> a /= 0)

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((x,y):t) = ((a*x),(b+y)) : mult (a,b) t

mult2 :: Monomio -> Polinomio -> Polinomio
mult2 (x,y) = map (\(a,b) -> (a*x,b+y))

--h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = aux h (normaliza t)
            where aux :: Monomio -> Polinomio -> Polinomio
                  aux (x,y) [] = [(x,y)]
                  aux (x,y) ((x1,y1):t) = if (y==y1) then ((x+x1),y) : t else (x1,y1) : (aux (x,y) t)

normaliza2 :: Polinomio -> Polinomio
normaliza2 l = nub $ map f l
  where
    f (a,b) = foldr (\(i,_) (z,k) -> (i+z,k)) (0,b) $ filter (\(x,y) -> y == b) l

--i)
soma :: Polinomio -> Polinomio -> Polinomio
soma [] x = x
soma (h:t) p = soma t (soma_aux h p)
                where soma_aux :: Monomio -> Polinomio -> Polinomio
                      soma_aux x [] = []
                      soma_aux (x,y) ((x1,y1):t) = if (y==y1) then ((x+x1),y) : t else (x1,y1) : (soma_aux (x,y) t)

soma2 :: Polinomio -> Polinomio -> Polinomio
soma2 p1 p2 = normaliza2 $ (++) p1 p2

--j)
produto :: Polinomio -> Polinomio -> Polinomio
produto p = concatMap (f p)
  where
    f p (a,b) = map (\(x,y) -> (a*x,y+b)) p

--k)
ordena :: Polinomio -> Polinomio
ordena = sortOn snd

--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = if length n1 /= length n2
                then False
                else and $ map (\(a,b) -> a == b) $ zip n1 n2
  where
    n1 = ordena $ normaliza2 p1
    n2 = ordena $ normaliza2 p2
