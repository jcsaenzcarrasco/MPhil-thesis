-- Knapsack non path tracking, strict evaluation

module Main where

import System.Random 
import System.Environment 
import Data.Array
import Nilsson 

knapsack :: Int -> Weight -> Int -> [(Value,Weight)] 
knapsack n wc s = ( sol!(n) ) 
 where
  sol :: Array Int [(Value,Weight)] 
  sol  = array (1,n) ([(1, chx (z!!0) ) ] ++ 
-- ~~~~~~~~~~ Stict, non path tracking functions
                       [(i, nchkS  (sol!(i-1)) (njnkS wc (z!!(i-1)) (sol!(i-1))) ) | i<-[2..n] ])

  chx :: (Value,Weight) -> [(Value,Weight)] 
  chx (v,w) = if w <= wc then [(v,w)] else zeroK 

  z :: [(Value,Weight)] 
  z  = zip (take n (randomValues s)) (take n (randomWeights (s+1)))

randomValues  seed = randomRs(1,20) (mkStdGen seed) 
randomWeights seed = randomRs(5,25)   (mkStdGen seed) 
