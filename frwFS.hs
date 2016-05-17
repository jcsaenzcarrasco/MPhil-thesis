-- ~~~~~~~~~~ Purely functional FRW, strict non path-tracking
module Main where

import Nilsson 
import Data.Array 
import Data.Graph 
import System.Environment 
import System.IO 

type Pair     = [(Capacity,Length)] 
type GraphFRW = Array Edge [(Capacity,Length)] 

frw :: GraphFRW -> GraphFRW 
frw g = foldr induct g (range (limitsG g)) 

induct :: Vertex -> GraphFRW -> GraphFRW
induct k g = mapG (const.update) g   
 where 
   update :: Edge -> Pair
-- ~~~~~~~~~~ Strict non path-tracking version ~~~~~
   update (x,y) = nchS  (g!(x,y)) (njnS (g!(x,k)) (g!(k,y)) )

mapG :: Ix a => (a -> b -> c) -> Array a b -> Array a c 
mapG f a = array (bounds a) [ (i, f i (a!i)) | i <- indices a ] 


-- ~~~~~~~~~~ Auxiliary functions

vertices :: GraphFRW -> [Vertex] 
vertices g = range (limitsG g) 

limitsG :: GraphFRW -> (Vertex,Vertex) 
limitsG g = (l,u) 
 where ((l,_),(u,_)) = bounds g 
