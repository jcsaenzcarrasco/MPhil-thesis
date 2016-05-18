{-
The following code include functions from David's King PhD thesis, specifically:
randomList, randomPerm, randomEdges, and randomGraph
Also, the auxiliary functions: applyST, mapST, listST

Remaining functions were derived in order to fulfill MCSP and Knapsack requirements
-}

module Graphs where

import Data.Array.ST 
import Control.Monad.ST 
import Data.Graph 
import System.Random 

randomPerm :: Int -> Int -> [Int] 
randomPerm n seed
   = runST (do { r <- newArray (1,n) 0  :: ST s (STArray s Int Int) ; 
                 applyST (\i -> writeArray r i i) [1..n]; 
                 applyST (swapArray r) (zip [1..n] (randomList n seed)); 
                 mapST (readArray r) [1..n] 
               }) 
      where
         swapArray r (x,y) = do { a <- readArray r x;
                                  b <- readArray r y; 
                                  writeArray r x b; 
                                  writeArray r y a 
                                }  

applyST :: (a -> ST s b) -> [a] -> ST s () 
applyST f []     = return () 
applyST f (x:xs) = do { f x; applyST f xs } 

mapST :: (a -> ST s b) -> [a] -> ST s [b] 
mapST f xs = listST (map f xs) 

listST :: [ST s a] -> ST s [a] 
listST = foldr consST nilST 
 where 
    nilST :: ST s [a] 
    nilST = return [] 

    consST :: ST s a -> ST s [a] -> ST s [a] 
    consST x xs = do { a  <- x; 
                       as <- xs; 
                       return (a:as) 
                     } 

randomList :: (Eq a, Num a, Random a) => a -> Int -> [a] 
randomList n seed = randomRs(1,n) (mkStdGen seed)

randomEdges :: Int -> Int -> Int -> [Edge]
randomEdges v e seed = take e [ (x+1, y+1) | r<-randomPerm (v*v) seed, (x, y) <- [r `divMod` v], x < y ]

fillEdges :: Int -> Int -> Int -> [Edge]
fillEdges v e seed = fillAux (zip [1..(v-1)] [2..v]) (mergesort (<) xs) 
 where 
  xs = randomEdges v e seed 

  fillAux [] ys = [] 
  fillAux xs [] = if null xs then []
                  else tail xs
  fillAux xxs@(x:xs) yys@(y:ys) 
     | fst x == fst y       = y : fillAux xxs ys 
     | (fst y - fst x) == 1 = y : fillAux  xs ys 
     | otherwise            = 
          if null xs then              fillAux [] ys
          else                head xs: fillAux xs yys 

randomGraph :: Int -> Int -> Int -> Graph
randomGraph v e seed = buildG (1, v) (randomEdges v e seed) 

randomGraphFilled :: Int -> Int -> Int -> Graph
randomGraphFilled v e seed = buildG (1, v) (fillEdges v e seed) 

splitm :: [a] -> ([a],[a]) 
splitm xs = go xs xs where 
  go (x:xs) (_:_:zs) = (x:us,vs) where (us,vs)=go xs zs 
  go    xs   _       = ([],xs) 

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a] 
merge pred xs []         = xs 
merge pred [] ys         = ys 
merge pred (x:xs) (y:ys) 
    | pred x y = x: merge pred xs (y:ys) 
    | otherwise = y: merge pred (x:xs) ys 

mergesort :: (a -> a -> Bool) -> [a] -> [a] 
mergesort pred []   = [] 
mergesort pred [x]  = [x] 
mergesort pred xs = merge pred (mergesort pred xs1) (mergesort pred xs2) 
  where 
    (xs1,xs2) = splitm xs 

