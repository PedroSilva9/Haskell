module Ficha4 where 
import Data.List
import Data.Char

type Nome = String 
type Agenda = [(Nome,[Contacto])]
data Contacto = Casa Integer
              | Trab Integer
              | Email String
              | Tlm Integer
              deriving Show

--3
emails :: [Contacto] -> [String]
emails [] = []
emails (Email x : t) = x : emails t
emails (_ : t) = emails t

--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n email [] = [(n, [Email email])]
acrescEmail n email ((a,b):t) | n == a = (a, Email email : b) : t
                              | otherwise = (a,b) : acrescEmail n email t

--b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((a,b):t) | x == nome = Just (emails b)
                         | x /= nome = verEmails n t

--c)
casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa nome ((x,c):t) | x == nome = casa x c
                    | x /= nome = casa nome t

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs c = 