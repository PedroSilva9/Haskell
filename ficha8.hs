module Ficha8 where

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt


calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = - (calcula a)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)

infixa :: ExpInt -> String
infixa (Const a) = show a
infixa (Simetrico a) = show (-(calcula a))
infixa (Mais a b) = show ((calcula a) + (calcula b))
infixa (Menos a b) = show ((calcula a) - (calcula b))
infixa (Mult a b) = show ((calcula a) * (calcula b))
{-
posfixa :: ExpInt -> String
posfixa (Const a) = [a]
posfixa (Simetrico a) = show ('-' : posfixa a)
posfixa (Mais a b) = show (posfixa a ++ posfixa b ++ '+')
posfixa (Menos a b) = show (posfixa a ++ posfixa b ++ '-')
posfixa (Mult a b) = show (posfixa a ++ posfixa b ++ '*') -}

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R x filhos) = x + sum (map soma filhos)

altura :: RTree a -> Int
altura (R x filhos) = 1 + myMaximum (map altura filhos)
                where myMaximum [] = 0
                      myMaximum l = maximum l

prune :: Int -> (RTree a) -> (RTree a)
prune 1 (R n _) = R n []
prune x (R n filhos) = R n (map (prune (x - 1)) filhos)

mirror :: RTree a -> RTree a
mirror (R x []) = R x []
mirror (R x filhos) = R x (map mirror (reverse filhos))

postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a filhos) = concat (map postorder filhos) ++ [a]  

--data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork l r) = (ltSum l) + (ltSum r)

listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork l r) = (listaLT l) ++ (listaLT r)

ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork l r) = 1 + (max (ltHeight l) (ltHeight r))

data FTree a b = Leaf b
               | No a (FTree a b) (FTree a b)

splitFTree :: FTree a b -> (BTree a, LTree b) 
splitFTree (Leaf x) = (Empty, Tip x)
splitFTree (No x l r) = let (l1,l2) = splitFTree l
                            (r1,r2) = splitFTree r
                        in  (Node x l r, Fork l2 r2)

{-joinTrees :: BTree a -> LTree a -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node x l r) (Fork l r) = Just --?????
joinTrees _ _ = Nothing - }




