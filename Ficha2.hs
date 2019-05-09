module Ficha2 where
import Data.List
import Data.Ord
import Data.Char

--EX1

--a
funA :: [Float] -> Float
funA [] = 0
funA (x:y) = x^2 + (funA y)
{-  funA [2,3,5,1]
	2^2 + (funA [3,5,1])
	4 + (funA [3,5,1])
	4 + (3^2 + (funA [5,1]))
	4 + (9 + (funA [5,1]))
	4 + (9 + (5^2 +(funA [1])))
	4 + (9 + (25 +(funA [1])))
	4 + (9 + (25 +(1^2 + funA [])))
	4 + (9 + (25 +(1 + funA [])))
	4 + (9 + (25 +(1 + 0)))
-}

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t) else (funB t)
{-  funB [8,5,12]
	8 : (funB [5,12])
	8 : (funB [12])
	8 : (12 : (funB []))
	8 : (12 : ([]))
	8 : 12
	[8,12]
-}

{-funC (x:y:t) = funC t
funC [x] = []
funC [] = []
  funC [1,2,3,4,5]
	funC [3,4,5]
	funC [5]
	funC []
	[]
-}

{-	funD "otrec"
g 1 [] = 1
g 1 (h:t) = g (h:l) t
g ("o":[]) "trec"
g "to" "rec"
g "rto" "ec"
g "erto" "c"
g "certo" []
"certo"
-}

{-|
Module : Ficha2
Description : Módulo correspondente à ficha 2.
Copyright : Aluno1 <xxxxxxx1@alunos.uminho.pt>

-}


--EX2

--a) A função dobro calcula a lista de dobros da lista recebida como argumentos.
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h*2) : dobros t

dobros2 :: [Float] -> [Float]
dobros2 = map (*2)

--b) Calcula o número de vezes que um caracter ocorre numa string.
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (h:t) = if c == h then 1 + numOcorre c t else numOcorre c t

numOcorre2 :: Char -> String -> Int
numOcorre2 c str = length $ filter (==c) str

--c) Testa se uma lista só tem elementos positivos.
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if (h > 0) then positivos t else False

positivos2 :: [Int] -> Bool
positivos2 = all (>0)

--d) Retira todos os elementos negativos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h >= 0) then h : soPos t else soPos t

soPos2 :: [Int] -> [Int]
soPos2 = filter (>0)

--e) Soma todos os números negativos da lista de entrada.
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h <= 0) then h + somaNeg t else somaNeg t

somaNeg2 :: [Int] -> Int
somaNeg2 l= sum $ filter (>0) l

--f) Devolve os últimos três elementos de uma lista. Se a lista de entrada tiver menos de três elementos, devolve a própria lista.
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x] = [x]
tresUlt [x,y] = [x,y]
tresUlt [x,y,z] = [x,y,z]
tresUlt (h:t) = tresUlt t

tresUlt2 :: [a] -> [a]
tresUlt2 l = if n < 3 then l else drop (n-3) l
  where
    n = length l

--g) Recebe uma lista de pares e devolve a lista com as primeiras componentes desses pares.
primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros ((a,b):t) = a : primeiros t

primeiros2 :: [(a,b)] -> [a]
primeiros2 = map fst


--EX3

--a) Recebe uma lista de caracteres, e selecciona dessa lista os caracteres que são algarismos.
soDigitos :: [Char] -> [Char]
soDigitos [] = [] --digito E [0,9]
soDigitos (h:t) = if (ord h >= ord '0') && (ord h <= ord '9') then h : soDigitos t else soDigitos t

soDigitos2 :: [Char] -> [Char]
soDigitos2 = filter isDigit

--b) Recebe uma lista de caracteres, e conta quantos desses caracteres são letras minúsculas.
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if (ord h >= ord 'a') && (ord h <= ord 'z') then 1 + minusculas t else minusculas t

minusculas2 :: [Char] -> Int
minusculas2 l = length $ filter isAsciiUpper l

--c) Recebe uma string e devolve uma lista com os algarismos que occorem nessa string, pela mesma ordem.
nums :: String -> [Int]
nums [] = []
nums (h:t) = if (ord h >= ord '0') && (ord h <= ord '9') then (ord h - ord '0') : nums t else nums t

nums2 :: String -> [Int]
nums2 l = map digitToInt $ filter isNumber l


--EX4

--a) Calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t

segundos2 :: [(a,b)] -> [b]
segundos2 = map snd

--b) Testa se um elemento aparece na lista como primeira componente de algum dos pares.
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x (h:t) = if (x == fst h) then True else nosPrimeiros x t

nosPrimeiros2 :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros2 x l = elem x $ map fst l

--c) Calcula a primeira menor parte.
minFst2 :: (Ord a) => [(a,b)] -> a
minFst2 l = minimum $ map fst l

--d)
sndMinFst :: (Ord a) => [(a,b)] -> a
sndMinFst l = head . tail . sort $ map fst l


mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h:t) = replicate y h : mygroup (drop (y-1) t)
  where
    y = aux 1 1 h t
    aux _ tot _ [] = tot
    aux 0 tot _ _ = tot
    aux control tot h (x:xs) = if h == x then aux control (tot+1) h xs else aux 0 tot h xs

