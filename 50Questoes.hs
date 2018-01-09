module Questoes50 where
import Data.Char
import Data.List


--1) Constrói a lista dos números inteiros compreendidos entre dois limites.
psenumFromTo :: Int -> Int -> [Int]
psenumFromTo x y | x == y = x : []
                 | x < y = x : psenumFromTo (x+1) y
                 | otherwise = []

--2) Constrói a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante.

psenumFromThenTo :: Int -> Int -> Int -> [Int]
psenumFromThenTo x y z | x == z = x : []
                       | x < z = x : psenumFromThenTo y (y + (y-x)) z
                       | x > z = x : psenumFromThenTo y (y + (y-x)) z 


--3) Concatena duas listas.
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) l = h : ((+++) t l)


--4) Dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (assume-se que o primeiro elemento se encontra na posição 0).
(!!!) :: [a] -> Int -> a 
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1) 


--5) Dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.
psreverse :: [a] -> [a] 
psreverse [] = []
psreverse (h:t) = aux h (psreverse t)
          where aux :: a -> [a] -> [a]
                aux x [] = [x]
                aux x (h:t) = h : aux x t


--6) Dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l. 
pstake :: Int -> [a] -> [a]
pstake 0 _ = []
pstake _ [] = []
pstake x (h:t) = if x > 0 then h : pstake (x-1) t else [] 


--7) Dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l.
psdrop :: Int -> [a] -> [a]
psdrop 0 lista = lista
psdrop _ [] = []
psdrop x (h:t) = psdrop (x-1) t


--8) Constói uma lista de pares a partir de duas listas.
pszip :: [a] -> [b] -> [(a,b)]
pszip (h1:t1) (h2:t2) = (h1,h2) : pszip t1 t2
pszip _ _ = []


--9) Testa se um elemento ocorre numa lista.
pselem :: (Eq a) => a -> [a] -> Bool
pselem _ [] = False
pselem x (h:t) = if x /= h then pselem x t else True


--10) Dado um inteiro n e um elemento x constói uma lista com n elementos, todos iguais a x. 
psreplicate :: Int -> a -> [a]
psreplicate 0 _ = []
psreplicate z y = if z > 0 then y : psreplicate (z-1) y else []


--11) Dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é intercalado entre os elementos da lista fornecida.
psinterspace :: a -> [a] -> [a]
psinterspace _ [] = []
psinterspace _ [x] = [x] --como só põe "_" entre 2 elementos não pode ser executável numa lista de apenas um elemento
psinterspace x (h:t) = h : ( x : psinterspace x t ) 


--12) Agrupa elementos iguais e consecutivos de uma lista.
psgroup :: Eq a => [a] -> [[a]]
psgroup [] = [] 
psgroup l = aaa : psgroup (psdrop (length aaa) l)
    where aaa = psgroup_aux l

psgroup_aux :: Eq a => [a] -> [a]
psgroup_aux [x] = [x]
psgroup_aux (x:h:t) = if x == h  then x : psgroup_aux (h:t) else [x] 


--13) Concatena as listas de uma lista.
psconcat :: [[a]] -> [a]
psconcat [] = []
psconcat (h:t) = (+++) h (psconcat t)


--14) Calcula a lista dos prefixos de uma lista.
psinits :: [a] -> [[a]]
psinits l = psinits_aux 0 l

psinits_aux :: Int -> [a] -> [[a]]
psinits_aux _ [] = [[]]
psinits_aux x l = if x < length l then pstake x l : psinits_aux (x+1) l else [l] 


--15) Calcula a lista de sufixos de uma lista.
pstails :: [a] -> [[a]]
pstails l = pstails_aux 0 l

pstails_aux :: Int -> [a] -> [[a]]
pstails_aux _ [] = [[]]
pstails_aux x l = if x < length l then psdrop x l : pstails_aux (x+1) l else []


--16) Testa se uma lista é prefixo de outra.
psisprefixof :: Eq a => [a] -> [a] -> Bool
psisprefixof [] _ = True
psisprefixof _ [] = False
psisprefixof (h1:t1) (h2:t2) = if h1 == h2 then psisprefixof t1 t2 else False


--17) Testa se uma lista é sufixo de outra.
psissuffixof :: Eq a => [a] -> [a] -> Bool
psissuffixof _ [] = False
psissuffixof [] _ = True
psissuffixof l1 l2 = psisprefixof (psreverse l1) (psreverse l2)


--18) Testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa.
psissubsequenceof :: Eq a => [a] -> [a] -> Bool
psissubsequenceof _ [] = False
psissubsequenceof [] _ = True
psissubsequenceof (x:xs) (y:ys) = if x == y then psissubsequenceof xs ys else psissubsequenceof (x:xs) ys


--19) Calcula a lista de posições em que um dado elemento ocorre numa lista.
pselemindices :: Eq a => a -> [a] -> [Int]
pselemindices _ [] = []
pselemindices x l = pselemindices_aux 0 x l

pselemindices_aux :: Eq a => Int -> a -> [a] -> [Int]
pselemindices_aux _ _ [] = []
pselemindices_aux i x (h:t) = if x == h then i : pselemindices_aux (i+1) x t else pselemindices_aux (i+1) x t


--20) Calcula uma lista com os mesmos elementos da recebida, sem repetições. --????
psnub :: Eq a => [a] -> [a] 
psnub [] = []
psnub (h:t) = h : psnub (psnub_aux h t)

psnub_aux :: Eq a => a -> [a] -> [a]
psnub_aux _ [] = []
psnub_aux x (h:t) = if h==x then (psnub_aux x t) else (h : psnub_aux x t)


--21) Retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento de uma lista.
psdelete :: Eq a => a -> [a] -> [a]
psdelete _ [] = []
psdelete x (h:t) = if x == h then t else h : psdelete x t


--22) Retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira. 
doubleslash :: Eq a => [a] -> [a] -> [a]
doubleslash l [] = l
doubleslash [] _ = []
doubleslash l1 (h:t) = (doubleslash (psdelete h l1) t) --caso não haja h em l1 o que acontece


--23) Retorna a lista resultante de acrescentar à primeira lista os elementos da segunda que não ocorrem na primeira.
psunion :: Eq a => [a] -> [a] -> [a]
psunion l [] = l
psunion [] l = l
psunion l (h:t) = if pselem h l then psunion l t else psunion ((+++) l [h]) t


--24) Retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda.
psintersect :: Eq a => [a] -> [a] -> [a]
psintersect _ [] = []
psintersect [] _ = []
psintersect l (h:t) = if pselem h l then h : psintersect l t else psintersect l t


--25) Dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.
psinsert :: Ord a => a -> [a] -> [a]
psinsert x [] = [x]
psinsert x (h:t) = if x > h then (h : psinsert x t) else (+++) [x] (h:t)


--26) Junta todas as strings da lista numa só, separando-as por um espaço.
psunwords :: [String] -> String 
psunwords [] = ""
psunwords [x] = x  
psunwords (h:t) = (+++) ((+++) h " ") (psunwords t) 


--27) Junta todas as strings da lista numa só, separando-as pelo caracter '\n'.
psunlines :: [String] -> String
psunlines [] = ""
psunlines [x] = (+++) x "\n"
psunlines (h:t) = (+++) ((+++) h ['\n']) (psunlines t)


--28) Dada uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. 
--    As posiçõesda lista começam em 0, i.e., a função deverá retornar 0 se o primeiro elemento da lista for o maior.
psMaior :: Ord a => [a] -> a 
psMaior [] = undefined 
psMaior (h:t) = psMaior_aux h t

psMaior_aux :: Ord a => a -> [a] -> a
psMaior_aux x [] = x 
psMaior_aux x (h:t) = if x <= h then psMaior_aux h t else psMaior_aux x t

--29) Testa se uma lista tem elementos repetidos.
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if pselem h t then True else temRepetidos t


--30) Determina a lista dos algarismos de uma dada lista de caracteres.
algarismos :: [Char] -> [Char]
algarismos [] = [] 
algarismos (h:t) = if isDigit (h) then h:algarismos t else algarismos t


--31) Determina os elementos de uma lista que ocorrem em posições ímpares.
--    Considere que o primeiro elemento da lista ocorre na posição 0 e por isso é par.
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (a:b:c) = b : posImpares c


--32) Determina os elementos de uma lista que ocorrem em posições pares.
--    Considere que o primeiro elemento da lista ocorre na posição 0 e por isso é par.
posPares :: [a] -> [a]
posPares [] = []
posPares (x:y:t) = x : posPares t


--33) Testa se uma lista está ordenada por ordem crescente. ERRO!!!!
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:t) = if x < y && isSorted (y:t) then True else False


--34) Calcula o resultado de ordenar uma lista. ERRO!!!!
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = psinsert h (iSort t)


--35) dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo a ordem lexicográfica.
menor :: String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor l1 l2 = length l2 > length l1 


--36) Testa se um elemento pertence a um multi-conjunto.
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet x (h:t) = if x == (fst h) then True else elemMSet x t


--37) Calcula o tamanho de um multi-conjunto.
erronome :: [(a,Int)] -> Int --erro no nome
erronome [] = 0
erronome ((a,b):t) = b + erronome t


--38) Converte um multi-conjuto na lista dos seus elementos.
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = (+++) (psreplicate b a) (converteMSet t)


--39) Acrescenta um elemento a um multi-conjunto.
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) = if x == a then (a,(b+1)) : t else (a,b) : insereMSet x t


--40) Que remove um elemento a um multi-conjunto. Se o elemento não existir, deve ser 
--    retornado o multi-conjunto recebido.
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t) = if x == a 
                         then 
                            if b == 1 then t else (a,(b-1)) : t
                         else (a,b) : removeMSet x t


--41) Dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos.
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = constroiMSet_aux [] l

constroiMSet_aux :: Eq a => [(a,Int)] -> [a] -> [(a,Int)]
constroiMSet_aux x [] = x
constroiMSet_aux x (h:t) = constroiMSet_aux (insereMSet h x) t 


{--42) Divide uma lista de Either s em duas listas.
pspartitionEithers :: [Either a b] -> ([a],[b])
pspartitionEithers


partitionEithers2 :: [Either a b] -> ([a],[b])
partitionEithers2 l = aux l ([],[])
where
aux :: [Either a b] -> ([a],[b]) -> ([a],[b])
aux ((Left h):t) (l1,l2) = aux t (h:l1,l2)
aux ((Right h):t) (l1,l2) = aux t (l1,h:l2)
aux [] ls = ls -}

--43)
catmaybe :: [Maybe a] -> [a]
catmaybe [] = []
catmaybe (Just x:t) = x : catmaybe t
catmaybe (Nothing : t) = catmaybe t 

--44)
data Movimento = Norte | Sul | Este | Oeste
              deriving Show 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao x [] = x
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t

--45)
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1 > x2 =  Oeste : caminho (x1-1, y1) (x2, y2)
                        | x1 < x2 =  Este : caminho (x1+1, y1) (x2, y2)
                        | y1 > y2 =  Sul : caminho (x1, y1-1) (x2, y2)
                        | y1 < y2 =  Norte : caminho (x1, y1+1) (x2, y2)
                        | otherwise = []


--46)
vertical :: [Movimento] -> Bool
vertical [] = True 
vertical (Norte:t) = True && vertical t
vertical (Sul:t) = True && vertical t
vertical (Este:t) = False && vertical t
vertical (Oeste:t) = False && vertical t

--47)
data Posicao = Pos Int Int
               deriving Show 

maisCentral :: [Posicao] -> Posicao
maisCentral [] = Pos 0 0
maisCentral (h:t) = maisCentral_aux t h
    where maisCentral_aux :: [Posicao] -> Posicao -> Posicao
          maisCentral_aux [] max = max
          maisCentral_aux (Pos x y:t) (Pos x1 y1) = if x1^2 + y1^2 > x^2 + y^2 
                                                    then maisCentral_aux t (Pos x y) --posiçao mais perto da origem
                                                    else maisCentral_aux t (Pos x1 y1) 


--48)
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1):t) | (y == y1) && (x == (x1 +1)) = (Pos x1 y1) : vizinhos (Pos x y) t 
                                   | (y == y1) && (x == (x1 -1)) = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | (x == x1) && (y == (y1 +1)) = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | (x == x1) && (y == (y1 -1)) = (Pos x1 y1) : vizinhos (Pos x y)  t
                                   | otherwise = vizinhos (Pos x y) t


--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2): t) = if x1 == x2 then mesmaOrdenada ((Pos x2 y2):t) else False


--50)
data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = interseccaoOK_aux l False
    where interseccaoOK_aux :: [Semaforo] -> Bool -> Bool
          interseccaoOK_aux [] _ = True
          interseccaoOK_aux (Vermelho:t) v = interseccaoOK_aux t v --espera pelo resto da lista para saber a veracidade
          interseccaoOK_aux (_ :t) False = interseccaoOK_aux t True --algo que não vermelho muda a veracidade para True
          interseccaoOK_aux (_ :t) True = False -- truque para chamar a funçao e mudar a veracidade para falso