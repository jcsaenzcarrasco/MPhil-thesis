-- ~~~~~~~~~~ Monadic FRW, strict non path-tracking
module Main where 

import Nilsson 
import Data.Array 
import System.Environment 
import Control.Monad.ST 
import Data.Array.ST 

type MyA s = STArray s (Int,Int) [(Capacity,Length)] 

frw n xs = runST $ 
  do {
   xa <- nLA ((1,1),(n,n)) xs; 
   xb <- nLA ((1,1),(n,n)) [];
   let loop(k,i,j) = 
         if k > n 
         then return () 
         else 
            if i > (n-1) 
            then do  
               transfer xb xa n 1 2 
               loop(k+1,1,2)  
            else 
               if j > n 
               then loop (k,i+1,i+2) 
               else do 
                       compute xa xb k i j 
                       loop(k,i,j+1) 
   in loop(1,1,2);
   getElems xa   }

compute :: MyA s -> MyA s -> Int -> Int -> Int -> ST s () 
compute xa xb k i j = 
   do 
      ij <- readArray xa (i,j) 
      ik <- readArray xa (i,k) 
      kj <- readArray xa (k,j)
-- ~~~~~~~~~~ Strict non path tracking version ~~~~~
      writeArray xb (i,j) (nch ij (njn ik kj)) 


-- ~~~~~~~~~~ Auxiliary functions 

nLA :: (Ix i) => (i,i) -> [e] -> ST s ((STArray s) i e) 
nLA = newListArray 

transfer :: MyA s -> MyA s -> Int -> Int -> Int -> ST s () 
transfer xb xa n i j = 
  if i>(n-1) 
  then return () 
  else 
     if j>n 
     then transfer xb xa n (i+1) (i+2) 
     else do 
            tba xb xa i j 
            transfer xb xa n i (j+1) 

tba :: MyA s -> MyA s -> Int -> Int -> ST s () 
tba xb xa i j =  
  do  
       ij <- readArray xb (i,j) 
       writeArray      xa (i,j) ij 
