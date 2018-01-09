module Ficha9 where

data Frac = F Integer Integer
{-
mdc :: Integer -> Integer -> Integer
mdc x y | mod x y /= 0 = mdc y (mod x y)
        | otherwise = abs y

normaliza :: Frac -> Frac
normaliza (F x y) = F (x `div` h) (y `div` h)
                where h = (mdc (abs x) (abs y))

instance Eq Frac where
    f1 == f2 = (a==x) && (b==y)
            where (F a b) = normaliza f1
                  (F x y) = normaliza f2

instance Ord Frac where
    f1 <= f2 = undefined

instance Show Frac where
    show (F a b) = undefined

 fun :: (Ord a,Num a) => a -> [a] -> [a]
 fun f l = filter p l
        where p x = x > 2*f


data  Exp a = Const a
            | Simetrico (Exp a)
            | Mais (Exp a) (Exp a)
            | Menos (Exp a) (Exp a)
            | Mult (Exp a) (Exp a)

calcula :: (Exp a) -> Int
calcula (Const n) = n
calcula (Simetrico e) = (calcula e) * (-1)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)

infixx :: (Exp a) -> String
infixx (Const n) = show n
infixx (Simetrico x) = "(-" ++ (infixx x) ++ ")"
infixx (Mais x y) = "(" ++ (infixx x) ++ " + " ++ (infixx y) ++ ")"
infixx (Menos x y) = "(" ++ (infixx x) ++ " - " ++ (infixx y) ++ ")"
infixx (Mult x y) = "(" ++ (infixx x) ++ " x " ++ (infixx y) ++ ")"

instance Show a => Show (Exp a) where
    show x = infixx x
-}

