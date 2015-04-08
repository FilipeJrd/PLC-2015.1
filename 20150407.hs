--------------------------------------- AULA 4/7/2015 -----------------------------------------
{-
type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

Pessoa "José" 22
Pessoa "Maria" 23

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " -- " ++ show a

Pessoa :: Nome -> Idade -> Pessoas
-}

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle f) = 3.1415926535897932384626433832795 * f * f
area (Rectangle c l) = c*l

data Dia = Sabado | Domingo | Segunda Int [String] | Terca Int [String] 
           | Quarta Int [String] | Quinta Int [String] | Sexta Int [String]

fimDeSemana :: Dia -> String
fimDeSemana Sabado = "Fim De Semana"
fimDeSemana Domingo = "Fim De Semana"
fimDeSemana _ = "Nao é fim de semana"

plcDia :: Dia -> Bool
plcDia Sabado = False
plcDia Domingo = False
plcDia (Segunda _ a) =  existe a "PLC"
plcDia (Terca _ a)=  existe a "PLC"
plcDia (Quarta _ a)=  existe a "PLC"
plcDia (Quinta _ a)=  existe a "PLC"
plcDia (Sexta _ a) =  existe a "PLC"

existe :: [String] -> String -> Bool
existe [] s = False
existe (x:xs) s | x == s = True
                | otherwise = existe xs s

data Expr = Lit Int | Add Expr Expr| Sub Expr Expr
eval :: Expr -> Int
eval (Lit n ) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

data Tree t = NilT | Node t (Tree t) (Tree t) deriving ( Eq,Ord,Show)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "("++(showExpr e1)++")" ++ "+" ++"("++(showExpr e2)++")"
showExpr (Sub e1 e2) = "("++(showExpr e1)++")" ++ "-" ++ "("++(showExpr e2)++")"

data List t = Nil | Cons t (List t) deriving(Show)

toList :: List t  -> [t]
toList Nil = []
toList (Cons a l) = [a]++ toList l


fromList :: [t] -> List t
fromList [] = Nil

from (x:xs) | xs ==[] = Cons x Nil
 			| otherwise =Cons x (fromList xs)

depth :: Tree t ->  [t]
depth NilT = []
depth (Node a (b) (c)) = [a] ++ depth b ++ depth c 