module ProgramacaoFuncional where

--3

matriz1 = [[2,4,0,6],
           [0,3,3,1],
           [0,1,3,0],
           [0,0,0,3]
          ]

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
