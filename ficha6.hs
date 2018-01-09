module Ficha6 where 

import Data.Char
import Data.List

psany :: (a->Bool) -> [a] -> Bool
psany p [] = False
psany p (h:t) = p h || psany p t

psanyOdd :: [Int] -> Bool
psanyOdd [] = False
psanyOdd (h:t) = odd h || psanyOdd t

mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile p [] = []
mytakeWhile p (h:t) | p h = h : mytakeWhile p t
                    | otherwise = []

mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile p [] = []
mydropWhile p (h:t) | p h = mydropWhile p t
                    | otherwise = h:t

myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith p _ [] = []
myzipWith p [] _ = []
myzipWith p (h:t) (h1:t1) =(p h h1) : (myzipWith p t t1)

mydeleteBy :: (a->a->Bool) -> a -> [a] -> [a]
mydeleteBy p x [] = []
mydeleteBy p x (h:t) | p x h = t
                     | otherwise = h : mydeleteBy p x t

mysortOn :: Ord b => (a->b) -> [a] -> [a]
mysortOn p [] = []
mysortOn p (h:t) = myinsert h (mysortOn p t)
                where myinsert x [] = [x]
                      myinsert x (y:ys) | p x <= p y = x:y:ys
                                        | otherwise = y : myinsert x ys

myspan :: (a->Bool) -> [a] -> ([a],[a])
myspan _ [] = ([],[])
myspan p (h:t) = if (p h) then (h:a,b) else ([],(h:t))
            where (a,b) = myspan p t

type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau grau poli = filter aux poli
                where aux :: Monomio -> Bool
                      aux (x,y) = y==grau

conta :: Int -> Polinomio -> Int 
conta grau poli = length (filter (\(x,y)->y==grau) poli)

{-mesma coisa mas com foldr.
 conta grau poli = foldr aux 0 poli
                 where aux (x,y) r = if y == grau then 1 + r else r -}

grau :: Polinomio -> Int 
grau poli = maximum (map snd poli)

deriv :: Polinomio -> Polinomio
deriv poli = (map aux poli)
          where aux :: Monomio -> Monomio
                aux (x,y) | y > 0 = ((x * fromIntegral y),(y-1))
                          | otherwise = (0,0)

calcula :: Float -> Polinomio -> Float
calcula n poli = sum (map (\(x,y)->x*n^y) poli)

simp :: Polinomio -> Polinomio
simp poli = filter (\(x,y)-> y/=0) poli
{-
mult :: Monomio -> Polinomio -> Polinomio
mult mono (h:t) = foldr (*) mono fromIntegral h : mult t -}

mult2 :: Monomio -> Polinomio -> Polinomio
mult2 (c,e) poli = map (\(x,y)-> (x * c , fromIntegral e + y)) poli

ordena :: Polinomio -> Polinomio
ordena poli = mysortOn snd poli

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = acresAux h (normaliza t)
            where acresAux :: Monomio -> Polinomio -> Polinomio
                  acresAux (c,e) [] = [(c,e)]
                  acresAux (c,e) ((x,y):t) | e == y = (c+x,e) : t
                                           | otherwise = (x,y) : (acresAux (c,e) t)

{-normaliza2 :: Polinomio -> Polinomio
normaliza2 (h:t) = foldr p h t : normaliza2 t
            where p :: Monomio -> Polinomio -> Polinomio
                  p (c,e) [] = [(c,e)]
                  p (c,e) ((x,y):t) | e == y = (c+x,e) : t
                                               | otherwise = (x,y) : (p (c,e) t) -}

soma :: Polinomio -> Polinomio -> Polinomio
soma [] poli = poli
soma poli [] = poli
soma (h:t) poli = soma t (acresAux h poli) 
           where acresAux :: Monomio -> Polinomio -> Polinomio
                 acresAux (c,e) [] = [(c,e)]
                 acresAux (c,e) ((x,y):t) | e == y = (c+x,e) : t
                                          | otherwise = (x,y) : (acresAux (c,e) t)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (h:t) poli = soma (produtoAux h poli)  (produto t poli)
                where produtoAux :: Monomio -> Polinomio -> Polinomio
                      produtoAux _ [] = []
                      produtoAux (c,e) ((a,b):t) = (c * a, e + b) : produtoAux (c,e) t

equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True 
equiv poli poli2 | h == h1 = equiv t t1
                 | otherwise = False
              where (h:t) = ordena poli 
                    (h1:t1) = ordena poli2

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (h:t) = aux (length h) t
        where aux :: Int -> [[a]] -> Bool
              aux x [] = True
              aux x (h:t) | x == length h = aux x t
                          | otherwise = False

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t) = (length h, length (h:t))

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat matriz matriz2 = zipWith (zipWith (+)) matriz matriz2
{-
mytranspose :: Mat a -> Mat a
mytranspose [] = []
mytranspose (h:t) = (map aux (h:t)) : mytranspose t
              where aux :: Mat a -> [a]
                    aux [] = []
                    aux ((h:t):t1) = h : aux t1 -}