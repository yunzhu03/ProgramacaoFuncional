{-# LANGUAGE FlexibleContexts #-}
{- |
Module      : PF_fichas
Description : Árvores binárias com conteúdo nos nós
Copyright   : LingYun Zhu <a100820@alunos.uminho.pt>;          

Resolução da ficha 6 de Programação Funcional por mim.
Nota: Pode estar tudo mal.
-}

module PF_fichas where

--1.
data BTree a = Empty
             | Node a (BTree a) (BTree a)
          deriving Show

arv1 :: Num a => BTree a 
arv1 = Node 2 (Node 3 (Node 7 Empty (Node 5 Empty Empty)) (Node 6 (Node 2 Empty Empty) Empty)) (Node 3 Empty Empty) 

--a)
altura :: BTree a -> Int 
altura Empty = 0
altura (Node a e d) = 1 + max (altura e) (altura d)


--b)
contaNodos :: BTree a -> Int 
contaNodos Empty = 0
contaNodos (Node a e d) = 1 + contaNodos e + contaNodos d


--c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a e     d    ) = folhas e + folhas d

{-
 * "==" precisa "Eq Btree a"  
@
folhas (Node a e d) | e == Empty && d == Empty = 1 
                    | otherwise = folhas e + folhas d
@
-}

--d)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune n (Node a e d) | n == 0    = Empty 
                     | otherwise = Node a (prune (n-1) e) (prune (n-1) d)

--e)
path :: [Bool] -> BTree a -> [a]
path [] (Node a _ _) = [a]
path _ Empty = []
path (True :t) (Node a e d) = a : path t d
path (False:t) (Node a e d) = a : path t e    

--f)
mirror :: BTree a -> BTree a 
mirror Empty = Empty 
mirror (Node a e d) = Node a (mirror d) (mirror e)

--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a e1 d1) (Node b e2 d2) = Node (f a b) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--h)
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a x1 x2, Node b y1 y2, Node c z1 z2)
   where
      (x1,y1,z1) = unzipBT e 
      (x2,y2,z2) = unzipBT d

--2.

--a)
{- Árvore binária de procura = 
   Node a e d
          e < a
          d > a
-}
-- Assumindo que a árvore não é vazia
minimo :: Ord a => BTree a -> a
minimo Empty = error "n.d."
minimo (Node a Empty _) = a
minimo (Node a e d    ) = minimo e

--b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = error "n.d."
semMinimo (Node a Empty d) = d
semMinimo (Node a e d    ) = Node a (semMinimo e) d

--c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin Empty = error "n.d."
minSmin (Node a Empty d) = (a, d)
minSmin (Node a e d) = (x, Node a y d)
   where
      (x,y) = minSmin e

--d)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty 
remove n (Node a e Empty) | n == a    = e
                          | otherwise = Node a e Empty
remove n (Node a e d) | n == a    = Node x e y
                      | n <  a    = Node a   (remove n e) d
                      | otherwise = Node a e (remove n d)
   where
      (x,y) = minSmin d

--3.
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
   deriving Show

type Turma = BTree Aluno --  ́arvore binária de procura (ordenada por n ́umero)

turma1 :: Turma 
turma1 = Node (12,"Nini",ORD,Aprov 14) (Node (7,"Nene",TE,Rep) Empty (Node (9,"Nionio",TE,Aprov 12) Empty Empty)) (Node (15,"Nana",TE,Rep) (Node (22,"Nono",TE,Rep) Empty (Node (23,"Nunu",TE,Faltou) Empty Empty)) Empty)

--a)
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (nr,_,_,_) e d) | n == nr = True 
                                 | n <  nr = inscNum n e
                                 | n >  nr = inscNum n d
                                 | otherwise = error "Não há otherwise"

--b)
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,no,_,_) e d) | n == no = True 
                                  | otherwise = inscNome n e || inscNome n d

--c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nr,no,TE,_) e d) = (nr,no) : trabEst e ++ trabEst d
trabEst (Node _ e d)  = trabEst e ++ trabEst d

--d)
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (nr,_,_,c) e d) | n == nr   = Just c
                             | n <  nr   = nota n e 
                             | otherwise = nota n d  

--e)
rebeldes :: Turma -> (Int,Int)
rebeldes Empty = (0,0)
rebeldes (Node (_,_,_,c) e d) = case c of 
                                    Faltou -> (1+a1+a2,1+b1+b2)      
                                    _      -> (  a1+a2,1+b1+b2)
   where
      (a1,b1) = rebeldes e 
      (a2,b2) = rebeldes d 

percFaltas :: Turma -> Float
percFaltas t = (fromIntegral a/ fromIntegral b)*100
   where
      (a,b) = rebeldes t

--f)
notas :: Turma -> [Int]
notas Empty = []
notas (Node (_,_,_,c) e d) = case c of 
                                  Aprov n -> n : notas e ++ notas d
                                  _       ->     notas e ++ notas d

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov t     = m 
   where
      l = notas t
      m = fromIntegral (sum l) / fromIntegral (length l)

--g)
nerds :: Turma -> (Int,Int,Int)
nerds Empty = (0,0,0)
nerds (Node (_,_,_,c) e d) = case c of 
                                    Aprov n -> (1+a1+a2,  b1+b2,1+c1+c2)      
                                    Rep     -> (  a1+a2,1+b1+b2,1+c1+c2)
                                    _       -> (  a1+a2,  b1+b2,1+c1+c2)
   where
      (a1,b1,c1) = nerds e 
      (a2,b2,c2) = nerds d 

aprovAv :: Turma -> Float
aprovAv Empty = 100
aprovAv t     = (fromIntegral a / fromIntegral c) * 100
   where
      (a,b,c) = nerds t


