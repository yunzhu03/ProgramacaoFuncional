{- |
Module      : PF_fichas
Description : Outros tipos de árvores
Copyright   : LingYun Zhu <a100820@alunos.uminho.pt>;          

Resolução da ficha 7 de Programação Funcional por mim.
Nota: Pode estar tudo mal.
-}

module PF_fichas where 

--1.
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais      ExpInt ExpInt
            | Menos     ExpInt ExpInt
            | Mult      ExpInt ExpInt

ei1:: ExpInt
ei1 = Mais (Const 3) (Menos (Const 2) (Const 5))

--a)
calcula :: ExpInt -> Int
calcula (Const n) = n 
calcula (Simetrico e) = -calcula e
calcula (Mais  e1 e2) = calcula e1 + calcula e2 
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult  e1 e2) = calcula e1 * calcula e2

--b)
infixa :: ExpInt -> String
infixa (Const n) = show n
infixa (Simetrico e1   ) = "-" ++ infixa e1
infixa (Mais      e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"    
infixa (Menos     e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"  
infixa (Mult      e1 e2) = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")" 

--c)
posfixa :: ExpInt -> String
posfixa (Const n) = show n
posfixa (Simetrico e) = posfixa e ++ "-"
posfixa (Mais  e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult  e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " *"

--2.
data RTree a = R a [RTree a] deriving Show

rt1 :: Num a => RTree a 
rt1 = R 5 [ R 3 [], R 7 [R 5 [R 8 [], R 9 []]], R 13 []]

--a)
soma :: Num a => RTree a -> a 
soma (R x []) = x 
soma (R x l ) = x + sum (map soma l)

--b)
altura :: RTree a -> Int 
altura (R x []) = 1 
altura (R x l ) = 1 + maximum (map altura l)

--c)
prune :: Int -> RTree a -> RTree a
prune 1 (R x _) = R x []
prune n (R x l) | n < 1     = R x l
                | otherwise = R x (map (prune (n-1)) l)

--d)
mirror :: RTree a -> RTree a
mirror (R x []) = R x []
mirror (R x l ) = R x (map mirror (reverse l))

--e)
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l ) = concatMap postorder l ++ [x] 

--3.
data BTree a = Empty 
             | Node a (BTree a) (BTree a)

data LTree a = Tip a
             | Fork (LTree a) (LTree a)

lt1 :: Num a => LTree a 
lt1 = Fork (Fork (Tip 6) (Tip 11)) (Fork (Tip 13) (Fork (Tip 22) (Tip 32)))
--a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a 
ltSum (Fork e d) = ltSum e + ltSum d

--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d

--c)
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1 
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

--4.
data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b)
    deriving Show

--a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf a  ) = (Empty, Tip a)
splitFTree (No a e d) = (Node a a1 a2, Fork b1 b2)
    where
        (a1,b1) = splitFTree e
        (a2,b2) = splitFTree d

--b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty _   = Nothing 
joinTrees a b = Just (madeiras a b)
    
madeiras :: BTree a -> LTree b -> FTree a b
madeiras _ (Tip a) = Leaf a
madeiras (Node a e1 d1) (Fork e2 d2) = No a (madeiras e1 e2) (madeiras d1 d2)
madeiras _ _ = error " caso de incompatibilidade = Nothing "

bt1 :: Num a => BTree a 
bt1 = Node 11 (Node 5 Empty Empty) (Node 17 (Node 16 Empty Empty) (Node 23 Empty Empty))