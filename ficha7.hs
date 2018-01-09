module Ficha7 where

data BTree a = Empty 
             | Node a (BTree a) (BTree a)
           deriving Show 

arvore1 = (Node 3 (Node 5 (Node 7 Empty Empty) Empty) (Node 6 Empty Empty))
arvore2 = (Node 2 (Node 3 (Node 1 Empty Empty) Empty) Empty)

altura :: BTree a -> Int
altura Empty = 0
altura (Node x l r) = 1 + max (altura l) (altura r)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x l r) = 1 + contaNodos l + contaNodos r


folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node x l r) = folhas l + folhas r

prune :: Int -> BTree a -> BTree a
prune y Empty = Empty
prune 0 _ = Empty
prune y (Node x l r) = Node x (prune (y-1) l) (prune (y-1) r)  

path :: [Bool] -> BTree a -> [a]
path [] _ = []
path _ Empty = []
paht (h:t) (Node x l r) = if h then x : path t l else x : path t r

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT p Empty _ = Empty
zipWithBT p _ Empty = Empty
zipWithBT p (Node x l r) (Node y e d) = Node (p x y) (zipWithBT p l e) (zipWithBT p r d)

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) l r) = let (l1,l2,l3) = unzipBT l
                                 (r1,r2,r3) = unzipBT r
                              in (Node x l1 r1, Node y l2 r2, Node z l3 r3)

minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x l r) = minimo l

semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty
semMinimo (Node x Empty r) = r
semMinimo (Node x l r) = Node x (semMinimo l) r

minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node x l r) = let a = minimo l
                           bt = Node x (semMinimo l) r
                       in (a,bt)

{-remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x tree = if fst (minSmin tree) /= x 
                then (Node n (remove l) (remove r))
                else remove r
            where (Node n l r) = tree  -}


type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno 

a2 = (Node (855489,"Joao",TE,Rep)(Node (54684,"Antonio",TE,Rep) Empty Empty) Empty) 

inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False
inscNum num (Node (x,_,_,_) l r) | num == x = True 
                                 | num > x = inscNum num r 
                                 | num < x = inscNum num l

inscNome :: Nome -> Turma -> Bool
inscNome nome Empty = False
inscNome nome (Node (_,x,_,_) l r) | nome /= x = inscNome nome l || inscNome nome r
                                   | nome == x = True 

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (x,y,TE,w) l r) = (trabEst l) ++ [(x,y)] ++ trabEst r
trabEst (Node (_,_,_,_) l r) = trabEst l ++ trabEst r

nota :: Numero -> Turma -> Maybe Classificacao
nota num Empty = Nothing
nota num (Node (x,y,z,w) l r) = if num == x 
                                then Just w 
                                else if (num < x) 
                                     then nota num l 
                                     else nota num r

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas node = (percFaltas_Aux node) / (fromIntegral (contaNodos node)) * 100
            where percFaltas_Aux :: Turma -> Float
                  percFaltas_Aux (Node (_,_,_,Faltou) l r) = 1 + percFaltas_Aux l + percFaltas_Aux r 
                  percFaltas_Aux (Node (_,_,_,_) l r) = percFaltas_Aux l + percFaltas_Aux r

mediaAprov :: Turma -> Float
mediaAprov node = (notaAprov node) / (aprov node)

notaAprov :: Turma -> Float
notaAprov Empty = 0
notaAprov (Node (_,_,_,Aprov x) l r) = fromIntegral x + notaAprov l + notaAprov r
notaAprov (Node (_,_,_,_) l r) = notaAprov l + notaAprov r

aprov :: Turma -> Float
aprov Empty = 0
aprov (Node (_,_,_,Aprov x) l r) = 1 + aprov l + aprov r
aprov (Node (_,_,_,_) l r) = aprov l + aprov r

aprovAv :: Turma -> Float
aprovAv Empty = 0 
aprovAv (Node (_,_,_,Aprov x) l r) = 1 + aprovAv l + aprovAv r
aprovAv (Node (_,_,_,Rep) l r) = (-1) + aprovAv l + aprovAv r
aprovAv (Node (_,_,_,_) l r) = aprovAv l + aprovAv r