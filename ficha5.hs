{- |
Module      : PF_fichas
Description : Funções de ordem superior
Copyright   : LingYun Zhu <a100820@alunos.uminho.pt>;          

Resolução da ficha 5 de Programação Funcional por mim.
Nota: Pode estar tudo mal.
-}

module PF_fichas where 
    
--3

matriz1 :: [[Integer]]
matriz1 = [[2,4,0,6],
           [0,3,3,1],
           [0,1,3,0],
           [0,0,0,3]
          ]

matriz2 :: [[Integer]]
matriz2 = [[1,2,3,4,5],
           [2,2]
          ]

type Mat a = [[a]]

--a)

dimOk :: Mat a -> Bool 
dimOk [] = True
dimOk m = elemLinha y m 
    where 
        (x:xs) = m
        y = length x


elemLinha :: Int -> Mat a -> Bool 
elemLinha n [] = True
elemLinha n (x:xs) | n == length x = elemLinha n xs
                   | otherwise     = False 

--b)

dimMat :: Mat a -> (Int,Int)
dimMat m = maiorDim m (0,0)


maiorDim :: Mat a -> (Int,Int) -> (Int,Int)
maiorDim []     (a,b) = (a,b)
maiorDim (x:xs) (a,b) | y > a     = maiorDim xs (y,b+1) 
                      | otherwise = maiorDim xs (a,b+1)
    where
        y = length x

 --c)

 -- Admitindo que as duas matrizes tenham a mesma dimensão, temos:

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat ([]:t1) (_:t2) = [] : addMat t1 t2
addMat ((x:xs):t1) ((y:ys):t2) = (x+y: z):t3
    where
        (z:t3) = addMat (xs:t1) (ys:t2)
addMat _ _ = []

--d)
transpose :: Mat a -> Mat a 
transpose [] = []
transpose ([]:t) = transpose t
transpose l = c : transpose r
    where
        r = rabo l
        c = cabeca l

rabo :: Mat a -> Mat a
rabo = map tail 

cabeca :: Mat a -> [a]
cabeca = map head 

--e)
-- Admitindo que as duas matrizes tenham a mesma ordem.

multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat [] _ = []
multMat (x:xs) m1 = linhaMat x m2 : multMat xs m1
    where
        (a:b) = m1
        m2 = transpose m1

linhaMat :: Num a => [a] -> Mat a -> [a]
linhaMat _ [] = []
linhaMat l m = map (linhaColuna l) m

linhaColuna :: Num a => [a] -> [a] -> a
linhaColuna (x:xs) (y:ys) = (x*y) + linhaColuna xs ys
linhaColuna _ _ = 0

--f)
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f ([]:t1) (_:t2) = zipWMat f t1 t2
zipWMat f ((x1:x2):xs) ((y1:y2):ys) = (f x1 y1 : a) : as
    where
        (a:as) = zipWMat f (x2:xs) (y2:ys)
zipWMat _ _ _ = [[]]

--g)

triSup :: (Eq a, Num a) => Mat a -> Bool 
triSup [] = True 
triSup m = verdade (a:as) (n-1) && triSup (reverse t)
    where
        n = length m
        ((a:as):t)  = reverse m

verdade :: (Eq a, Num a ) => [a] -> Int -> Bool
verdade [] _ = False
verdade (x:xs) n | n > 0 && x == 0 = verdade xs (n-1)
                 | n == 0    = True
                 | otherwise = False

--h)
rotateLeft :: Mat a -> Mat a 
rotateLeft [[]] = [[]]
rotateLeft ([]:t) = []
rotateLeft m = map last m : rotateLeft (map init m)
