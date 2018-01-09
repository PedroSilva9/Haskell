module Fichas where

(><) :: Int -> Int -> Int
(><) x y | y > 0 = x + (><) x (y-1)
         | otherwise = 0  

psdiv :: Int -> Int -> Int
psdiv n m | n < m = 0
          | otherwise = 1 + psdiv (n-m) m

psmod :: Int -> Int -> Int
psmod n m | n < m = n
          | otherwise = psmod (n-m) m

power :: Int -> Int -> Int
power x y | y > 0 = x * power x (y-1)
          | otherwise = 1

type Polinomio = [Monomio]
type Monomio = (Float,Int)

poli = [(2.0,2),(3.0,1),(4.0,3),(3.0,5)]

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (h:t) | n == snd h = 1 + conta n t
              | otherwise = conta n t

grau :: Polinomio -> Int
grau l = maximum (map (\(a,b) -> b) l)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\(a,b) -> b==n) p

deriv :: Polinomio -> Polinomio
deriv p = map (\(a,b) -> ((a*fromIntegral b),(b-1)) ) p

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((a,b) : t) = (x*a)^fromIntegral b + calcula x t

simp :: Polinomio -> Polinomio
simp p = filter (\(a,b) -> a /= 0) p

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) p = map (\(a,b) -> (a*x,b+y)) p

soma :: Polinomio -> Polinomio -> Polinomio
soma [] x = x
soma (h:t) p = soma t (soma_aux h p)
                where soma_aux :: Monomio -> Polinomio -> Polinomio
                      soma_aux x [] = []
                      soma_aux (x,y) ((x1,y1):t) = if (y==y1) then ((x+x1),y) : t else (x1,y1) : (soma_aux (x,y) t)

normaliza :: Polinomio -> Polinomio -> Polinomio
normaliza p p1 = soma p p1

{-
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = produto_aux (normaliza p1) (normaliza p2)

produto_aux :: Polinomio -> Polinomio -> Polinomio
produto_aux [] x = []
produto_aux x [] = []
produto_aux ((a,b):t) ((a1,b1):t1) | b == b1 = (a*a1,b+b1) : produto t t1
                                   | otherwise = produto_aux (a,b) t1
-}
mysortOn :: Ord b => (a->b) -> [a] -> [a]
mysortOn p [] = []
mysortOn p (h:t) = myinsert h (mysortOn p t)
                where myinsert x [] = [x]
                      myinsert x (y:ys) | p x <= p y = x:y:ys
                                        | otherwise = y : myinsert x ys

ordena :: Polinomio -> Polinomio
ordena p = mysortOn snd p

--ficha 4

data Hora = H Int Int 
          deriving Show 

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

horas2min :: Hora -> Int
horas2min (H x y) = x*60 + y

--a
validaEtapa :: Etapa -> Bool
validaEtapa (h1, h2) = horas2min h1 < horas2min h2

--b
validaViagem :: Viagem -> Bool
validaViagem [] = True
validaViagem (h:t) = validaEtapa h && validaViagem t

--c
partidachegada :: Viagem -> Etapa
partidachegada [] = undefined
partidachegada (h:t) = (fst h , snd (head (reverse (h:t))))

--3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome,[Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome str [] = [(nome, [Email str])]
acrescEmail nome str ((a,b):t) | nome == a = (a,Email str : b) : t
                               | otherwise = (a,b) : acrescEmail nome str t

emails :: [Contacto] -> [String]
emails [] = []
emails (Email x : t) = x : emails t
emails (_ : t) = emails t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing 
verEmails nome ((a,b):t) | nome == a = Just (emails b)
                         | otherwise = verEmails nome t

consTelefs :: [Contacto] -> [Integer]