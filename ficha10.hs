module Ficha10 where

import System.Random
import Data.List
import Data.Char

bingo :: IO ()
bingo = do let l = [1..90]
           bingoAux l

bingoAux :: (Eq a, Show a) => [a] -> IO ()
bingoAux [] = return ()
bingoAux l = do i <- randomRIO (0, length l-1)
                putStrLn  ("Saiu: " ++ show (l!!i))
                getChar
                bingoAux (delete (l !! i) l)


segredo :: IO String
segredo = do x1 <- randomRIO ('0','9')
             x2 <- randomRIO ('0','9')
             x3 <- randomRIO ('0','9')
             x4 <- randomRIO ('0','9')
             return [x1,x2,x3,x4]
                
{-
OU 
segredo = sequence (replicate 4 (randomRIO('0','9')))
-}

pcorreta :: String -> String -> Int
pcorreta [] [] = 0
pcorreta (x:xs) (y:ys) | x == y = 1 + pcorreta xs ys
                       | x /= y = pcorreta xs ys

perrada :: String -> String -> Int
perrada i s = length (intersect i s) - pcorreta i s

ler :: IO String
ler = do putStr ("Introduza uma sequência de 4 números: ")
         l <- getLine
         if length l == 4 && all isDigit l
            then return l
            else ler


mastermind :: IO ()
mastermind = do s <- segredo 
                aux s
            where aux :: String -> IO ()
                  aux i = do d <- ler
                             if pcorreta d i == 4 
                                then putStrLn ("Ganhou")
                                else do print (pcorreta d i)
                                        print (perrada d i)
                                        aux i

{-
data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap l (x,y)) = (length l == 5) && (length (nub l) == length l) && (x/=y) && (maximum l <= 50) && (minimum l >= 0)

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l1 (x1,y1)) (Ap l2 (x2,y2)) = (aux1 0 l1 l2, aux2 0 (x1,x2) (x2,y2))

aux1 :: Int -> [Int] -> [Int] -> Int
aux1 n l l2 = 


leAposta :: IO Aposta
leAposta = do putStr "Aposta: "
              a <- getLine
              let ns = map read (words a)
              if (length ns /= 7) then leAposta 
                else return (Ap (take 6 ns) (n!!5,n!!6)) -}