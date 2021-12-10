{- |
Module      : PF_fichas
Description : Classes de tipos
Copyright   : LingYun Zhu <a100820@alunos.uminho.pt>;          

Resolução da ficha 8 de Programação Funcional por mim.
A maioria foi feita na aula por isso deve estar bem.
Nota: Pode estar tudo mal.
-}

module PF_fichas where 

--1.
data Frac = F Integer Integer
  

--a)
mdc :: Integer -> Integer -> Integer 
mdc x y | x == y = x 
        | x <  y = mdc  x   (y-x)
        | x >  y = mdc (x-y) y
        | otherwise = error "Só vai ser utilizada para valores positivos."

--Assumindo que y /= 0
normaliza :: Frac -> Frac
normaliza (F a b) | a > 0 && b < 0 = F (-div a c) (abs(div b c))
                  | a < 0 && b < 0 = F (abs(div a c)) (abs(div b c))
                  | otherwise = F (div a c) (div b c)
    where
        c = mdc (abs a) (abs b)

--b)
instance Eq Frac where 
    (F a b) == (F c d) = (a*d) == (b*c) -- Integer pertence Eq
    
{-
Resolução alternativa
    f1 == f2 = let (F a b) = normaliza f1
                   (F x y) = normaliza f2
               in a == x && b == y 
-}

--c)
instance Ord Frac where
    f1 <= f2 = let (F a b) = normaliza f1
                   (F c d) = normaliza f2 -- a função normaliza garante que o denominador é positivo
               in a*d <= c*b

--d)
instance Show Frac where
  --Show a => a -> String
    show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

--e)
instance Num Frac where
    (+) (F a b) (F c d) = normaliza (F (a*d+c*b) (b*d))
    (*) (F a b) (F c d) = normaliza (F (a*c) (b*d))
    (-) (F a b) (F c d) = normaliza (F (a*d-c*b) (b*d))
    negate f = F (-a) b
        where
            (F a b) = normaliza f
    abs f = F (abs a) b
        where
            (F a b) = normaliza f
    signum f | a < 0 = F (-1) 1
             | a > 0 = F 1 1
             | otherwise = F 0 1
        where
            (F a b) = normaliza f
    fromInteger n = F n 1

--f)
selecao :: Frac -> [Frac] -> [Frac]
selecao f = filter (>2*f) 

--2.
data Exp a = Const a
           | Simetrico     (Exp a)
           | Mais  (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult  (Exp a) (Exp a)

--a)
instance Show a => Show (Exp a) where 
    show (Const a) = show a
    show (Simetrico a) = "-" ++ "(" ++ show a ++ ")"
    show (Mais  a b) = "(" ++ show a ++ "+" ++ show b ++ ")" 
    show (Menos a b) = "(" ++ show a ++ "-" ++ show b ++ ")" 
    show (Mult  a b) = "(" ++ show a ++ "*" ++ show b ++ ")" 

--b)
calcula :: Num a => Exp a -> a
calcula (Const a) = a 
calcula (Simetrico a) = -(calcula a)
calcula (Mais  a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult  a b) = calcula a * calcula b

instance (Eq a, Num a) => Eq (Exp a) where
    a == b = calcula a == calcula b 

--c)
instance (Ord a, Num a) => Num (Exp a) where
    (+) = Mais
    (*) = Mult
    (-) = Menos 
    negate = Simetrico 
    abs a | calcula a < 0 = Simetrico a 
          | otherwise = a
    signum a | calcula a > 0 = Const 1
             | calcula a < 0 = Const (-1)
             | otherwise = Const 0
    fromInteger n = Const (fromInteger n)
    


