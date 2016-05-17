-- ~~~~~~~~~~ Purely functional SS, strict with path-tracking
module Main where 

import Nilsson 
import Graphs 
import Data.Array 
import System.Environment 

solutionSS :: Int -> Int -> Int -> [(Capacity,Length,Path)] 
solutionSS v e seed = sol ! 1
 where 
  sol = array (1,v) ([(v,onePath)    ] ++ 
                     [(i,chooses i (randomweights !! (i-1))) | i <- [v-1,v-2..1] ]) 

  randomgraph   = elems $ randomGraphFilled v e seed
-- ~~~~~~~~~~ 20 and 50 are the bounds for random values of capacity and length just as references
--            in order to avoid handling more parameters in solutionSS, but can be any Int value
  randomlist    = zip (randomList 20 seed) (randomList 50 (seed+1)) 
  randomweights = mixListsPairs randomgraph randomlist

  chooses :: Int -> [(Int,Int,Int)] -> [(Capacity,Length,Path)] 
  chooses _ []     = zeroPath 
-- ~~~~~~~~~~ Strict with path tracking
  chooses f (x:xs) = nchPS  (njnPS  [(snd3 x, trd3 x, [f,fst3 x])] (sol ! (fst3 x)) ) ( chooses f xs ) 

