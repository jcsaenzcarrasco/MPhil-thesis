-- ~~~~~~~~~~ Purely functional SS, strict non path-tracking
module Main where 

import Nilsson 
import Graphs 
import Data.Array 
import System.Environment 

solutionSS :: Int -> Int -> Int -> [(Capacity,Length)] 
solutionSS v e seed = sol ! 1
 where 
  sol = array (1,v) ([(v,oneNonPath) ] ++ 
                     [(i,chooses   (randomweights !! (i-1))) | i <- [v-1,v-2..1] ]) 

  randomgraph   = elems $ randomGraphFilled v e seed
-- ~~~~~~~~~~ 20 and 50 are the bounds for random values of capacity and length just as references
--            in order to avoid handling more parameters in solutionSS, but can be any Int value
  randomlist    = zip (randomList 20 seed) (randomList 50 (seed+1)) 
  randomweights = mixListsPairs randomgraph randomlist 

  chooses :: [(Int,Int,Int)] -> [(Capacity,Length)] 
  chooses []     = zeroNonPath
-- ~~~~~~~~~~ Strict non path tracking
  chooses (x:xs) = nchS (njnS [(snd3 x, trd3 x)] (sol ! (fst3 x)) ) ( chooses xs ) 
