module Ficha5 where
import Data.List
import Data.Char
--1
{-a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
  [6,12,18] 

b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
[2,4,6,8,10,12,14,16,18,20]
[6,12,18]  

c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
[(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]

d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]
 [1,1,4,4,9,9,16,16,25,25]
-}

--2 
{-
a = [2^x | x <- [0..10]]
b = [(x,y) | x <- [1..5] , y <-[1..5], x + y == 6 ]
c = [[1..x] | x <- [1..5]]
d = [replicate x 1 | x <- [1..5]]
d1= [[1 | y <- [1..x]] | x <- [1..5]]
e = [product [1..x] | x <- [1..6]] -}

--3
digitAlpha :: String -> (String,String)
digitAlpha [] = ("","")
digitAlpha (h:t) = let (a,b) = digitAlpha t
                    in if isDigit h 
                        then (h:a,b)
                        else (a,h:b)

--4
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) =  let (x,y,z) = nzp t
                in if h == 0
                then (x,y+1,z)
                else  if h > 0
                      then (x,y,z+1)
                      else (x+1,y,z)

--5
{-psdivMod :: Integral a -> a -> a -> (a,a)
psdivMod x y | x < y = (0,x)
             | otherwise = let (a,b) = psdivMod (x-y) y
                            in (1+a,b) -}

--6
psfromDigits :: [Int] -> Int 
psfromDigits [] = 0
psfromDigits l = psfromDigits_aux ((length l)-1) l

psfromDigits_aux :: a -> [Int] -> Int
psfromDigits_aux _ [] = 0
psfromDigits_aux x (h:t) = h*10^x + psfromDigits_aux (x-1) t