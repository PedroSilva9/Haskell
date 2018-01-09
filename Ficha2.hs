--FICHA2

module Ficha2 where
import Data.Char


--EX1

--a
funA :: [Float] -> Float --NAO DA COM DOUBLE !!!!
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
	[8,12] -}

{-funC (x:y:t) = funC t
funC [x] = []
funC [] = []
  funC [1,2,3,4,5]
	funC [3,4,5]
	funC [5]
	funC []
	[] -}

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

--b) Calcula o número de vezes que um caracter ocorre numa string.
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (h:t) = if (c == h) then 1 + numOcorre c t else numOcorre c t

--c) Testa se uma lista só tem elementos positivos.
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if (h > 0) then positivos t else False

--d) Retira todos os elementos negativos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h >= 0) then h : soPos t else soPos t

--e) Soma todos os números negativos da lista de entrada.
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h <= 0) then h + somaNeg t else somaNeg t

--f) Devolve os últimos três elementos de uma lista. Se a lista de entrada tiver menos de três elementos, devolve a própria lista. 
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x] = [x]
tresUlt [x,y] = [x,y] 
tresUlt [x,y,z] = [x,y,z]
tresUlt (h:t) = tresUlt t

--g) Recebe uma lista de pares e devolve a lista com as primeiras componentes desses pares.
primeiros :: [(a,b)] -> [a]
primeiros [] = []
primeiros ((a,b):t) = a : primeiros t


--EX3

--a) Recebe uma lista de caracteres, e selecciona dessa lista os caracteres que são algarismos.
soDigitos :: [Char] -> [Char]
soDigitos [] = [] --digito E [0,9]
soDigitos (h:t) = if (ord h >= ord '0') && (ord h <= ord '9') then h : soDigitos t else soDigitos t

--b) Recebe uma lista de caracteres, e conta quantos desses caracteres são letras minúsculas.
minusculas :: [Char] -> Int 
minusculas [] = 0
minusculas (h:t) = if (ord h >= ord 'a') && (ord h <= ord 'z') then 1 + minusculas t else minusculas t

--c) Recebe uma string e devolve uma lista com os algarismos que occorem nessa string, pela mesma ordem. 
nums :: String -> [Int]
nums [] = []
nums (h:t) = if (ord h >= ord '0') && (ord h <= ord '9') then (ord h - ord '0') : nums t else nums t 


--EX4

--a) Calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t 

--b) Testa se um elemento aparece na lista como primeira componente de algum dos pares. 
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x (h:t) = if (x == fst h) then True else nosPrimeiros x t

--c) Calcula a primeira menor parte.
minFst :: (Ord a) => [(a,b)] -> a
minFst [] = 0
minFst (x,y):t = if x < minFst (x,y) t then x else minFst (x,y) t  