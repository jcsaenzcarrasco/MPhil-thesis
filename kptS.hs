-- Knapsack with path tracking, strict evaluation

module Main where

import System.Random 
import System.Environment 
import Data.Array
import Nilsson 

knapsack :: Int -> Weight -> Int -> [(Value,Weight,Path)] 
knapsack n wc s = ( sol!(n) ) 
 where
  sol :: Array Int [(Value,Weight,Path)] 
  sol =  array (1,n) ([(1, chx (p2t (z!!0) 1)) ] ++
-- ~~~~~~~~~~ Strict with path tracking functions
                      [(i, nchkPS  (sol!(i-1)) (njnkPS wc (p2t (z!!(i-1)) i) (sol!(i-1))) ) | i<-[2..n] ])

  chx :: (Value,Weight,Path) -> [(Value,Weight,Path)] 
  chx (v,w,p) = if w <= wc then [(v,w,p)] else []

  z :: [(Value,Weight)] 
  z  = zip (take n (randomValues s)) (take n (randomWeights (s+1)))

randomValues  seed = randomRs(1,20) (mkStdGen seed) 
randomWeights seed = randomRs(5,25) (mkStdGen seed) 

-- p2t is abbreviation for pair to triple 
p2t :: (Value,Weight) -> Int -> (Value,Weight,Path) 
p2t (v,w) i = (v,w,[i]) 
